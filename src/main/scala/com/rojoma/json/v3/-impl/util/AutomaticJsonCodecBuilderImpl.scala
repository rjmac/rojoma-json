package com.rojoma.json.v3
package `-impl`.util

import scala.collection.mutable

import codec._
import util.{JsonKey, AlternativeJsonKey, JsonKeyStrategy, Strategy, LazyCodec, NullForNone, ForbidUnknownFields}

import MacroCompat._

class AutomaticJsonCodecBuilderImpl[Ctx <: Context](c_ : Ctx)  extends MacroCompat(c_) with MacroCommon[Ctx] {
  import c.universe._
  class Impl[T : c.WeakTypeTag] {
    private lazy val T = weakTypeOf[T]
    private lazy val Tname = TypeTree(T)

    private def identityStrat(x: String) = x
    private def underscoreStrat(x: String) = CamelSplit(x).map(_.toLowerCase).mkString("_")

    private def freshTermName(): TermName = toTermName(c.freshName())

    private def nameStrategy(thing: Symbol, default: String => String): String => String = {
      checkAnn(thing, typeOf[JsonKeyStrategy])
      thing.annotations.reverse.find { ann => isType(ann.tree.tpe, typeOf[JsonKeyStrategy]) } match {
        case Some(ann) =>
          findValue(ann) match {
            case Some(strat : Symbol) =>
              try {
                Strategy.valueOf(strat.name.decodedName.toString) match {
                  case Strategy.Identity =>
                    identityStrat _
                  case Strategy.Underscore =>
                    underscoreStrat _
                }
              } catch {
                case _: IllegalArgumentException =>
                  default
              }
            case _ =>
              default
          }
        case None =>
          default
      }
    }

    private val nfnDefault = {
      checkAnn(T.typeSymbol, typeOf[NullForNone])
      T.typeSymbol.annotations.exists { ann => isType(ann.tree.tpe, typeOf[NullForNone]) }
    }

    private val defaultNameStrategy = nameStrategy(T.typeSymbol, identityStrat)
    private val forbidUnknownFields = T.typeSymbol.annotations.exists { ann => isType(ann.tree.tpe, typeOf[ForbidUnknownFields]) }

    // since v2 and v3 share the same names for their annotations, warn if we find one that isn't
    // the same type but is the same name and don't find one that IS the right type.
    def checkAnn(param: Symbol, t: Type): Unit = {
      param.annotations.find { ann => ann.tree.tpe.erasure.typeSymbol.name == t.typeSymbol.name && !isType(ann.tree.tpe, t) }.foreach { ann =>
        if(!param.annotations.exists { ann => isType(ann.tree.tpe, t) }) {
          c.warning(posOf(param, ann), "Found non-v3 `" + t.typeSymbol.name.decodedName + "' annotation; did you accidentally import v2's?")
        }
      }
    }

    private def hasLazyAnnotation(param: TermSymbol) = {
      checkAnn(param, typeOf[LazyCodec])
      param.annotations.exists { ann => isType(ann.tree.tpe, typeOf[LazyCodec]) }
    }

    private def computeJsonNames(param: TermSymbol): Seq[String] = {
      var names = List(nameStrategy(param, defaultNameStrategy)(param.name.decodedName.toString))
      checkAnn(param, typeOf[JsonKey])
      checkAnn(param, typeOf[AlternativeJsonKey])

      val keyAnnotations = param.annotations.filter { ann => isType(ann.tree.tpe, typeOf[JsonKey]) }
      if(keyAnnotations.size > 1) {
        c.abort(posOf(param, keyAnnotations(1)), "Found multiple JsonKey annotations")
      }
      val keyAnnotation = keyAnnotations.headOption
      for(ann <- keyAnnotation)
        findValue(ann) match {
          case Some(s: String) =>
            names = List(s)
          case _ =>
            c.abort(posOf(param, ann), "Unable to find value for JsonKey annotation")
        }

      val alternativeKeyAnnotations = param.annotations.filter { ann => isType(ann.tree.tpe, typeOf[AlternativeJsonKey]) }
      for(ann <- alternativeKeyAnnotations)
        findValue(ann) match {
          case Some(s : String) =>
            names = s :: names
          case _ =>
            c.abort(posOf(param, ann), "Unable to find value for AlternativeJsonKey annotation")
        }

      names.reverse
    }

    private def findAccessor(param: TermSymbol) =
      param.name.asInstanceOf[TermName]

    private def findCodecType(param: TermSymbol) = {
      val tpe = param.typeSignature.asSeenFrom(T, T.typeSymbol)
      if(isType(tpe.erasure, typeOf[Option[_]].erasure)) {
        val TypeRef(_, _, c) = tpe
        c.head
      } else {
        tpe
      }
    }
    private def isOption(param: TermSymbol) = {
      val tpe = param.typeSignature.asSeenFrom(T, T.typeSymbol)
      isType(tpe.erasure, typeOf[Option[_]].erasure)
    }
    private def hasNullForNoneAnnotation(param: TermSymbol) = {
      checkAnn(param, typeOf[NullForNone])
      nfnDefault || param.annotations.exists(ann => isType(ann.tree.tpe, typeOf[NullForNone]))
    }

    private case class FieldInfo(encName: TermName, decName: TermName, isLazy: Boolean, jsonNames: Seq[String], accessorName: TermName, missingMethodName: TermName, errorAugmenterMethodName: TermName, codecType: Type, isOption: Boolean, isNullForNone: Boolean)

    private val fieldss = locally {
      val seenNames = new mutable.HashSet[String]
      val buffer = new mutable.ListBuffer[List[FieldInfo]]
      for {
        member <- T.members
        if member.isMethod && member.asMethod.isPrimaryConstructor
      } {
        val mem = member.asMethod
        if(mem.owner == T.typeSymbol) {
          for {
            params <- mem.paramLists
          } {
            if(params.isEmpty || !params.head.asTerm.isImplicit) {
              val fieldList =
                for { rawParam <- params }
                yield {
                  val param = rawParam.asTerm
                  val names = computeJsonNames(param)
                  names.find(seenNames) match {
                    case Some(name) =>
                      c.abort(param.pos, s"The name `$name' is already used by the codec for $Tname")
                    case None =>
                      seenNames ++= names
                  }
                  FieldInfo(
                    freshTermName(),
                    freshTermName(),
                    hasLazyAnnotation(param),
                    names,
                    findAccessor(param),
                    freshTermName(),
                    freshTermName(),
                    findCodecType(param),
                    isOption(param),
                    hasNullForNoneAnnotation(param)
                  )
                }
              buffer += fieldList
            }
          }
        }
      }
      buffer.toList
    }
    private val fields = fieldss.flatten

    // three names for temporary variables used during the encoding/decoding process
    private val tmp = freshTermName()
    private val tmp2 = freshTermName()
    private val tmp3 = freshTermName()
    private val tmp4 = freshTermName()
    private val lValue = freshTermName()
    private val rValue = freshTermName()
    private val expectedFields = freshTermName()

    // the name of the thing being encoded or decoded
    private val param = freshTermName()

    private def encodes = fields.map { fi =>
      val enc = q"_root_.com.rojoma.json.v3.codec.JsonEncode[${TypeTree(fi.codecType)}]"
      if(fi.isLazy) {
        q"private[this] lazy val ${fi.encName} = $enc"
      } else {
        q"private[this] val ${fi.encName} = $enc"
      }
    }

    private def encoder = locally {
      val encoderMap = freshTermName()
      def encoderMapUpdates = for(fi <- fields) yield {
        if(fi.isOption) {
          if(fi.isNullForNone) {
            q"""$encoderMap += ${fi.jsonNames.head} -> {
                  val $tmp = $param.${fi.accessorName}
                  if($tmp.isInstanceOf[_root_.scala.Some[_]]) ${fi.encName}.encode($tmp.get)
                  else _root_.com.rojoma.json.v3.ast.JNull
                }"""
          } else {
            q"""val $tmp = $param.${fi.accessorName}
                if($tmp.isInstanceOf[_root_.scala.Some[_]]) $encoderMap += ${fi.jsonNames.head} -> ${fi.encName}.encode($tmp.get)"""
          }
        } else {
          q"$encoderMap += ${fi.jsonNames.head} -> ${fi.encName}.encode($param.${fi.accessorName})"
        }
      }

      q"""def encode($param: $Tname) = {
            val $encoderMap = _root_.scala.collection.immutable.VectorMap.newBuilder[_root_.scala.Predef.String, _root_.com.rojoma.json.v3.ast.JValue]
            ..$encoderMapUpdates
            _root_.com.rojoma.json.v3.ast.JObject($encoderMap.result())
          }"""
    }

    private def decodes = fields.map { fi =>
      val dec = q"_root_.com.rojoma.json.v3.codec.JsonDecode[${TypeTree(fi.codecType)}]"
      if(fi.isLazy) {
        q"private[this] lazy val ${fi.decName} = $dec"
      } else {
        q"private[this] val ${fi.decName} = $dec"
      }
    }

    private def missingMethods: List[DefDef] = fields.filter(!_.isOption).map { fi =>
      q"""private[this] def ${fi.missingMethodName}: _root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Nothing] =
            _root_.scala.Left(_root_.com.rojoma.json.v3.codec.DecodeError.MissingField(${fi.jsonNames.head},
                              _root_.com.rojoma.json.v3.codec.Path.empty))"""
    }

    private def errorAugmenterMethods: List[DefDef] = fields.map { fi =>
      q"""private[this] def ${fi.errorAugmenterMethodName}($tmp3 : _root_.scala.Either[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Any], $tmp4 : _root_.scala.Predef.String): _root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Nothing] =
           _root_.scala.Left($lValue($tmp3.asInstanceOf[_root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Any]]).augment(_root_.com.rojoma.json.v3.codec.Path.Field($tmp4)))"""
    }

    private def fieldSet =
      if(forbidUnknownFields) {
        List(q"""private[this] val $expectedFields = _root_.scala.collection.immutable.Set(..${fields.flatMap(_.jsonNames)})""")
      } else {
        Nil
      }

    def decoder = locally {
      val obj = freshTermName()
      val tmps = fieldss.map { _.map { _ => freshTermName() } }

      val decoderMapExtractions: List[ValDef] = for((fi,tmp) <- fields.zip(tmps.flatten)) yield {
        val expr = if(fi.isOption) {
          fi.jsonNames.foldRight[Tree](q"""_root_.scala.None""") { (jsonName, otherwise) =>
            q"""val $tmp2 = $obj.get($jsonName)
                if($tmp2.isInstanceOf[_root_.scala.Some[_]]) {
                  val $tmp3 = ${fi.decName}.decode($tmp2.get)
                  if($tmp3.isInstanceOf[_root_.scala.Right[_,_]]) Some($rValue($tmp3.asInstanceOf[_root_.scala.Right[_root_.scala.Any, ${TypeTree(fi.codecType)}]]))
                  else if(_root_.com.rojoma.json.v3.ast.JNull == $tmp2.get) _root_.scala.None
                  else return ${fi.errorAugmenterMethodName}($tmp3, $jsonName)
                } else $otherwise"""
          }
        } else {
          fi.jsonNames.foldRight[Tree](q"""return ${fi.missingMethodName}""") { (jsonName, otherwise) =>
            q"""val $tmp2 = ${obj}.get($jsonName)
                if($tmp2.isInstanceOf[_root_.scala.Some[_]]) {
                  val $tmp3 = ${fi.decName}.decode($tmp2.get)
                  if($tmp3.isInstanceOf[_root_.scala.Right[_,_]]) $rValue($tmp3.asInstanceOf[_root_.scala.Right[_root_.scala.Any, ${TypeTree(fi.codecType)}]])
                  else return ${fi.errorAugmenterMethodName}($tmp3, $jsonName)
                } else $otherwise"""
          }
        }
        q"val $tmp = $expr"
      }
      val create = // not sure how to do this with quasiquote...
        tmps.foldLeft(Select(New(TypeTree(T)), toTermName("<init>")) : Tree) { (seed, arglist) =>
          Apply(seed, arglist.map(Ident(_)))
        }

      def checkUnknownFields=
        if(forbidUnknownFields) {
          List(q"""val $tmp = $obj.keysIterator.filterNot($expectedFields)
                   if($tmp.hasNext) {
                     return _root_.scala.Left(
                       _root_.com.rojoma.json.v3.codec.DecodeError.InvalidField($tmp.next())
                     )
                   }""")
        } else {
          Nil
        }

      q"""def decode($param: _root_.com.rojoma.json.v3.ast.JValue): _root_.scala.Either[_root_.com.rojoma.json.v3.codec.DecodeError, $Tname] =
            if($param.isInstanceOf[_root_.com.rojoma.json.v3.ast.JObject]) {
              val $obj = $param.asInstanceOf[_root_.com.rojoma.json.v3.ast.JObject].fields
              ..$decoderMapExtractions
              ..$checkUnknownFields
              _root_.scala.Right($create)
            } else {
              _root_.scala.Left(_root_.com.rojoma.json.v3.codec.DecodeError.InvalidType(
                _root_.com.rojoma.json.v3.ast.JObject,
                $param.jsonType,
                _root_.com.rojoma.json.v3.codec.Path.empty))
            }"""
    }

    private def lValueDef =
      if(EitherCompat.hasValue)
        q"""@_root_.scala.inline private[this] def $lValue[K,V](a: Left[K,V]) = a.value"""
      else
        q"""@_root_.scala.inline private[this] def $lValue[K,V](a: Left[K,V]) = a.a"""

    private def rValueDef =
      if(EitherCompat.hasValue)
        q"""@_root_.scala.inline private[this] def $rValue[K,V](b: Right[K,V]) = b.value"""
      else
        q"""@_root_.scala.inline private[this] def $rValue[K,V](b: Right[K,V]) = b.b"""

    private def eitherValues = List(lValueDef, rValueDef)

    def encode: c.Expr[JsonEncode[T] with JObjectEncode[T]] = {
      val tree =
        q"""(new _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.`-impl`.util.JObjectEncode[$Tname] {
              ..$encodes
              $encoder
              override def toString = ${"#<JsonEncode for " + T.toString + ">"}
            }) : _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.`-impl`.util.JObjectEncode[$Tname]"""

      // println(tree)

      c.Expr[JsonEncode[T] with JObjectEncode[T]](tree)
    }

    def decode: c.Expr[JsonDecode[T]] = {
      val tree =
        q"""(new _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname] {
              ..$eitherValues
              ..$decodes
              ..$missingMethods
              ..$errorAugmenterMethods
              ..$fieldSet
              $decoder
              override def toString = ${"#<JsonDecode for " + T.toString + ">"}
            }) : _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname]"""

      // println(tree)

      c.Expr[JsonDecode[T]](tree)
    }

    def codec: c.Expr[JsonEncode[T] with JsonDecode[T] with JObjectEncode[T]] = {
      val tree = q"""(new _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname] with _root_.com.rojoma.json.v3.`-impl`.util.JObjectEncode[$Tname] {
                       ..$eitherValues
                       ..$encodes
                       ..$decodes
                       ..$missingMethods
                       ..$errorAugmenterMethods
                       ..$fieldSet
                       $encoder
                       $decoder
                       override def toString = ${"#<JsonCodec for " + T.toString + ">"}
                     }) : _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname] with _root_.com.rojoma.json.v3.`-impl`.util.JObjectEncode[$Tname]"""

      // println(tree)

      c.Expr[JsonEncode[T] with JsonDecode[T] with JObjectEncode[T]](tree)
    }
  }
}

object AutomaticJsonCodecBuilderImpl {
  def encode[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonEncode[T] with JObjectEncode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[ctx.type](ctx)
    new b.Impl[T]().encode
  }

  def decode[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonDecode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[ctx.type](ctx)
    new b.Impl[T]().decode
  }

  def codec[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonEncode[T] with JsonDecode[T] with JObjectEncode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[ctx.type](ctx)
    new b.Impl[T]().codec
  }
}

