package com.rojoma.json.v3
package `-impl`.util

import scala.collection.mutable

import codec._
import util.{JsonKey, JsonKeyStrategy, Strategy, LazyCodec, NullForNone}

import MacroCompat._

abstract class AutomaticJsonCodecBuilderImpl[T] extends MacroCompat {
  val c: Context
  import c.universe._

  implicit def Ttag: c.WeakTypeTag[T]
  private lazy val T = weakTypeOf[T]
  private lazy val Tname = TypeTree(T)

  private def identityStrat(x: String) = x
  private def underscoreStrat(x: String) = CamelSplit(x).map(_.toLowerCase).mkString("_")

  private def freshTermName(): TermName = toTermName(c.freshName())

  private def isType(t: Type, w: Type) = {
    // There HAS to be a better way to do this.
    // t MAY be <error>.  w must not be!
    // since <error> =:= any type, reject if it looks "impossible".
    t =:= w && !(t =:= typeOf[String] && t =:= typeOf[Map[_,_]])
  }

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

  private lazy val defaultNameStrategy = nameStrategy(T.typeSymbol, identityStrat)

  // since v2 and v3 share the same names for their annotations, warn if we find one that isn't
  // the same type but is the same name and don't find one that IS the right type.
  def checkAnn(param: Symbol, t: Type) {
    param.annotations.find { ann => ann.tree.tpe.erasure.typeSymbol.name == t.typeSymbol.name && !isType(ann.tree.tpe, t) }.foreach { ann =>
      if(!param.annotations.exists { ann => isType(ann.tree.tpe, t) }) {
        c.warning(param.pos, "Found non-v3 `" + t.typeSymbol.name.decodedName + "' annotation; did you accidentally import v2's?")
      }
    }
  }

  private def hasLazyAnnotation(param: TermSymbol) = {
    checkAnn(param, typeOf[LazyCodec])
    param.annotations.exists { ann => isType(ann.tree.tpe, typeOf[LazyCodec]) }
  }

  private def computeJsonName(param: TermSymbol): String = {
    var name = nameStrategy(param, defaultNameStrategy)(param.name.decodedName.toString)
    checkAnn(param, typeOf[JsonKey])
    for(ann <- param.annotations if isType(ann.tree.tpe, typeOf[JsonKey]))
      findValue(ann) match {
        case Some(s: String) =>
          name = s
        case _ =>
          // pass
      }
    name
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
  private def hasNullForNameAnnotation(param: TermSymbol) = {
    checkAnn(param, typeOf[NullForNone])
    param.annotations.exists(ann => isType(ann.tree.tpe, typeOf[NullForNone]))
  }

  private case class FieldInfo(codecName: TermName, isLazy: Boolean, jsonName: String, accessorName: TermName, missingMethodName: TermName, errorAugmenterMethodName: TermName, codecType: Type, isOption: Boolean, isNullForNone: Boolean)

  private lazy val fieldss = locally {
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
                val name = computeJsonName(param)
                if(seenNames(name)) {
                  c.abort(param.pos, s"The name `$name' is already used by the codec for $Tname")
                } else seenNames += name
                FieldInfo(
                  freshTermName(),
                  hasLazyAnnotation(param),
                  name,
                  findAccessor(param),
                  freshTermName(),
                  freshTermName(),
                  findCodecType(param),
                  isOption(param),
                  hasNullForNameAnnotation(param)
                )
              }
            buffer += fieldList
          }
        }
      }
    }
    buffer.toList
  }
  private lazy val fields = fieldss.flatten

  // TODO: figure out how to add the "lazy" modifiers after the fact
  private lazy val encodes = fields.map { fi =>
    val enc = q"_root_.com.rojoma.json.v3.codec.JsonEncode[${TypeTree(fi.codecType)}]"
    if(fi.isLazy) {
      q"private[this] lazy val ${fi.codecName} = $enc"
    } else {
      q"private[this] val ${fi.codecName} = $enc"
    }
  }
  private lazy val decodes = fields.map { fi =>
    val dec = q"_root_.com.rojoma.json.v3.codec.JsonDecode[${TypeTree(fi.codecType)}]"
    if(fi.isLazy) {
      q"private[this] lazy val ${fi.codecName} = $dec"
    } else {
      q"private[this] val ${fi.codecName} = $dec"
    }
  }

  private def encode: c.Expr[JsonEncode[T]] = {
    val tmp = freshTermName()
    val tmp2 = freshTermName()

    val param = freshTermName()
    val encoderMap = freshTermName()
    val encoderMapUpdates = for(fi <- fields) yield {
      if(fi.isOption) {
        if(fi.isNullForNone) {
          q"""$encoderMap(${fi.jsonName}) = {
                val $tmp = $param.${fi.accessorName}
                if($tmp.isInstanceOf[_root_.scala.Some[_]]) ${fi.codecName}.encode($tmp.get)
                else _root_.com.rojoma.json.v3.ast.JNull
              }"""
        } else {
          q"""val $tmp = $param.${fi.accessorName}
              if($tmp.isInstanceOf[_root_.scala.Some[_]]) $encoderMap(${fi.jsonName}) = ${fi.codecName}.encode($tmp.get)"""
        }
      } else {
        q"$encoderMap(${fi.jsonName}) = ${fi.codecName}.encode($param.${fi.accessorName})"
      }
    }

    val encoder = q"""def encode($param: $Tname) = {
                        val $encoderMap = new _root_.scala.collection.mutable.LinkedHashMap[_root_.scala.Predef.String, _root_.com.rojoma.json.v3.ast.JValue]
                        ..$encoderMapUpdates
                        _root_.com.rojoma.json.v3.ast.JObject($encoderMap)
                      }"""

    val tree =
      q"""(new _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] {
            ..$encodes
            $encoder
            override def toString = "#<JsonEncode for " + ${T.toString} + ">"
          }) : _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname]"""

    // println(tree)

    c.Expr[JsonEncode[T]](tree)
  }

  private def decode: c.Expr[JsonDecode[T]] = {
    val tmp = freshTermName()
    val tmp2 = freshTermName()
    val tmp3 = freshTermName()

    val param = freshTermName()
    val obj = freshTermName()
    val tmps = fieldss.map { _.map { _ => freshTermName() } }

    val missingMethods: List[DefDef] = fields.filter(!_.isOption).map { fi =>
      q"""private[this] def ${fi.missingMethodName}: _root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Nothing] =
            _root_.scala.Left(_root_.com.rojoma.json.v3.codec.DecodeError.MissingField(${fi.jsonName},
                              _root_.com.rojoma.json.v3.codec.Path.empty))"""
    }

    val errorAugmenterMethods: List[DefDef] = fields.map { fi =>
      q"""private[this] def ${fi.errorAugmenterMethodName}($tmp3 : _root_.scala.Either[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Any]): _root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Nothing] =
           _root_.scala.Left($tmp3.asInstanceOf[_root_.scala.Left[_root_.com.rojoma.json.v3.codec.DecodeError, _root_.scala.Any]].a.augment(_root_.com.rojoma.json.v3.codec.Path.Field(${fi.jsonName})))"""
    }

    val decoderMapExtractions: List[ValDef] = for((fi,tmp) <- fields.zip(tmps.flatten)) yield {
      val expr = if(fi.isOption) {
        q"""val $tmp2 = $obj.get(${fi.jsonName})
            if($tmp2.isInstanceOf[_root_.scala.Some[_]]) {
              val $tmp3 = ${fi.codecName}.decode($tmp2.get)
              if($tmp3.isInstanceOf[_root_.scala.Right[_,_]]) Some($tmp3.asInstanceOf[_root_.scala.Right[_root_.scala.Any, ${TypeTree(fi.codecType)}]].b)
              else if(_root_.com.rojoma.json.v3.ast.JNull == $tmp2.get) _root_.scala.None
              else return ${fi.errorAugmenterMethodName}($tmp3)
            } else _root_.scala.None"""
      } else {
        q"""val $tmp2 = ${obj}.get(${fi.jsonName})
            if($tmp2.isInstanceOf[_root_.scala.Some[_]]) {
              val $tmp3 = ${fi.codecName}.decode($tmp2.get)
              if($tmp3.isInstanceOf[_root_.scala.Right[_,_]]) $tmp3.asInstanceOf[_root_.scala.Right[_root_.scala.Any, ${TypeTree(fi.codecType)}]].b
              else return ${fi.errorAugmenterMethodName}($tmp3)
            } else return ${fi.missingMethodName}"""
      }
      q"val $tmp = $expr"
    }
    val create = // not sure how to do this with quasiquote...
      tmps.foldLeft(Select(New(TypeTree(T)), toTermName("<init>")) : Tree) { (seed, arglist) =>
        Apply(seed, arglist.map(Ident(_)))
      }

    val decoder = q"""def decode($param: _root_.com.rojoma.json.v3.ast.JValue): _root_.scala.Either[_root_.com.rojoma.json.v3.codec.DecodeError, $Tname] =
                        if($param.isInstanceOf[_root_.com.rojoma.json.v3.ast.JObject]) {
                          val $obj = $param.asInstanceOf[_root_.com.rojoma.json.v3.ast.JObject].fields
                          ..$decoderMapExtractions
                          _root_.scala.Right($create)
                        } else {
                          _root_.scala.Left(_root_.com.rojoma.json.v3.codec.DecodeError.InvalidType(
                            _root_.com.rojoma.json.v3.ast.JObject,
                            $param.jsonType,
                            _root_.com.rojoma.json.v3.codec.Path.empty))
                        }"""

    val tree =
      q"""(new _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname] {
            ..$decodes
            ..$missingMethods
            ..$errorAugmenterMethods
            $decoder
            val acceptTypes = _root_.com.rojoma.json.v3.`-impl`.util.CommonAcceptTypes.justJObject
            override def toString = "#<JsonDecode for " + ${T.toString} + ">"
          }) : _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname]"""

    // println(tree)

    c.Expr[JsonDecode[T]](tree)
  }

  private def codec: c.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val encode = toTermName("encode")
    val decode = toTermName("decode")
    val x = toTermName("x")

    val tree = q""" ((
new _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname] {
  private[this] val $encode = _root_.com.rojoma.json.v3.util.AutomaticJsonEncodeBuilder[$Tname]
  private[this] val $decode = _root_.com.rojoma.json.v3.util.AutomaticJsonDecodeBuilder[$Tname]

  def encode($x : $Tname) = $encode.encode($x)
  def decode($x : _root_.com.rojoma.json.v3.ast.JValue) = $decode.decode($x)
  def acceptTypes = $decode.acceptTypes
  override def toString = "#<JsonCodec for " + ${T.toString} + ">"
} ) : _root_.com.rojoma.json.v3.codec.JsonEncode[$Tname] with _root_.com.rojoma.json.v3.codec.JsonDecode[$Tname])
"""

    // println(tree)

    c.Expr[JsonEncode[T] with JsonDecode[T]](tree)
  }
}

object AutomaticJsonCodecBuilderImpl {
  // ...and typechecking falls over.

  def encode[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonEncode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.encode.asInstanceOf[ctx.Expr[JsonEncode[T]]]
  }

  def decode[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonDecode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.decode.asInstanceOf[ctx.Expr[JsonDecode[T]]]
  }

  def codec[T : ctx.WeakTypeTag](ctx: Context): ctx.Expr[JsonEncode[T] with JsonDecode[T]] = {
    val b = new AutomaticJsonCodecBuilderImpl[T] {
      val c = ctx
      val Ttag = implicitly[ctx.WeakTypeTag[T]].asInstanceOf[c.WeakTypeTag[T]]
    }
    b.codec.asInstanceOf[ctx.Expr[JsonEncode[T] with JsonDecode[T]]]
  }
}

