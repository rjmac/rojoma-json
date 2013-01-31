package com.rojoma.`json-impl`.util

import scala.collection.mutable

import scala.reflect.macros.Context

import com.rojoma.json.codec.JsonCodec
import com.rojoma.json.util.{JsonKey, JsonKeyStrategy, Strategy, LazyCodec, NullForNone}

object AutomaticJsonCodecBuilderImpl {
  def impl[T : c.WeakTypeTag](c: Context): c.Expr[JsonCodec[T]] = {
    import c.universe._

    val T = weakTypeOf[T]
    val Tname = TypeTree(T)

    def identityStrat(x: String) = x
    def underscoreStrat(x: String) = CamelSplit(x).map(_.toLowerCase).mkString("_")

    def isType(t: Type, w: Type) =
      // There HAS to be a better way to do this.
      // t MAY be <error>.  w must not be!
      // since <error> =:= any type, reject if it looks "impossible".
      t =:= w && !(t =:= typeOf[String] && t =:= typeOf[Map[_,_]])

    def nameStrategy(thing: Symbol, default: String => String) = {
      thing.annotations.reverse.find { ann => isType(ann.tpe, typeOf[JsonKeyStrategy]) } match {
        case Some(ann) =>
          ann.javaArgs.get(TermName("value")) match {
            case Some(LiteralArgument(Constant(arg : Symbol))) =>
              try {
                Strategy.valueOf(arg.name.decoded) match {
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

    val defaultNameStrategy = nameStrategy(T.typeSymbol, identityStrat)

    def hasLazyAnnotation(param: TermSymbol) =
      param.annotations.exists { ann => isType(ann.tpe, typeOf[LazyCodec]) }

    def computeJsonName(param: TermSymbol): String = {
      var name = nameStrategy(param, defaultNameStrategy)(param.name.decoded)
      for(ann <- param.annotations if isType(ann.tpe, typeOf[JsonKey]))
        ann.javaArgs.get(TermName("value")) match {
          case Some(LiteralArgument(Constant(s: String))) =>
            name = s
          case _ =>
            // pass
        }
      name
    }
    def findAccessor(param: TermSymbol) =
      param.name.asInstanceOf[TermName]

    def findCodecType(param: TermSymbol) = {
      val tpe = param.typeSignature.asSeenFrom(T, T.typeSymbol)
      if(isType(tpe.erasure, typeOf[Option[_]].erasure)) {
        val TypeRef(_, _, c) = tpe
        c.head
      } else {
        tpe
      }
    }
    def isOption(param: TermSymbol) = {
      val tpe = param.typeSignature.asSeenFrom(T, T.typeSymbol)
      isType(tpe.erasure, typeOf[Option[_]].erasure)
    }
    def hasNullForNameAnnotation(param: TermSymbol) =
      param.annotations.exists(ann => isType(ann.tpe, typeOf[NullForNone]))

    case class FieldInfo(codecName: TermName, isLazy: Boolean, jsonName: String, accessorName: TermName, codecType: Type, isOption: Boolean, isNullForNone: Boolean)

    val seenNames = new mutable.HashSet[String]
    val fieldss = locally {
      val buffer = new mutable.ListBuffer[List[FieldInfo]]
      for {
        member <- T.members
        if member.isMethod && member.asMethod.isPrimaryConstructor
      } {
        val mem = member.asMethod
        if(mem.owner == T.typeSymbol) {
          for {
            params <- mem.paramss
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
                    c.freshName(),
                    hasLazyAnnotation(param),
                    name,
                    findAccessor(param),
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
    val fields = fieldss.flatten

    val codecs = fields.map { fi =>
      // TODO: figure out how to add the "lazy" modifier after the fact
      val codec = q"_root_.com.rojoma.json.codec.JsonCodec[${TypeTree(fi.codecType)}]"
      if(fi.isLazy) q"private[this] lazy val ${fi.codecName} = $codec"
      else q"private[this] val ${fi.codecName} = $codec"
    }

    val tmp: TermName = c.freshName()
    val tmp2: TermName = c.freshName()
    val tmp3: TermName = c.freshName()

    val param: TermName = c.freshName()
    val encoderMap: TermName = c.freshName()
    val encoderMapUpdates = for(fi <- fields) yield {
      if(fi.isOption) {
        if(fi.isNullForNone) {
          q"""$encoderMap(${fi.jsonName}) = {
                // Hm, doesn't look like q can generate a match statement with holes yet
                // (it looks like it always tries to use the variables as constants)
                val $tmp = $param.${fi.accessorName}
                if($tmp.isDefined) ${fi.codecName}.encode($tmp.get)
                else com.rojoma.json.ast.JNull
              }"""
        } else {
          q"""val $tmp = $param.${fi.accessorName}
              if($tmp.isDefined) $encoderMap(${fi.jsonName}) = ${fi.codecName}.encode($tmp.get)"""
        }
      } else {
        q"$encoderMap(${fi.jsonName}) = ${fi.codecName}.encode($param.${fi.accessorName})"
      }
    }
    val encoder = q"""def encode($param: $Tname) = {
                        val $encoderMap = new _root_.scala.collection.mutable.LinkedHashMap[_root_.scala.Predef.String, _root_.com.rojoma.json.ast.JValue]
                        ..$encoderMapUpdates
                        _root_.com.rojoma.json.ast.JObject($encoderMap)
                      }"""

    val obj: TermName = c.freshName()
    val tmps = fieldss.map { _.map { _ => TermName(c.freshName()) } }
    val decoderMapExtractions: List[ValDef] = for((fi,tmp) <- fields.zip(tmps.flatten)) yield {
      val expr = if(fi.isOption) {
        q"""val $tmp2 = $obj.get(${fi.jsonName})
            if($tmp2.isDefined) {
              val $tmp3 = ${fi.codecName}.decode($tmp2.get)
              if($tmp3.isDefined) $tmp3
              else if($tmp2.get == _root_.com.rojoma.json.ast.JNull) _root_.scala.None else return _root_.scala.None
            } else _root_.scala.None"""
      } else {
        q"""val $tmp2 = ${obj}.get(${fi.jsonName})
            if($tmp2.isDefined) {
              val $tmp3 = ${fi.codecName}.decode($tmp2.get)
              if($tmp3.isDefined) $tmp3.get
              else return _root_.scala.None
            } else return _root_.scala.None"""
      }
      q"val $tmp = $expr"
    }
    val create = // not sure how to do this with quasiquote...
      tmps.foldLeft(Select(New(TypeTree(T)), TermName("<init>")) : Tree) { (seed, arglist) =>
        Apply(seed, arglist.map(Ident(_)))
      }
    val decoder = q"""def decode($param: _root_.com.rojoma.json.ast.JValue): _root_.scala.Option[$Tname] =
                        if($param.isInstanceOf[_root_.com.rojoma.json.ast.JObject]) {
                          val $obj = $param.asInstanceOf[_root_.com.rojoma.json.ast.JObject].fields
                          ..$decoderMapExtractions
                          _root_.scala.Some($create)
                        } else {
                          _root_.scala.None
                        }"""

    val tree =
      q"""(new _root_.com.rojoma.json.codec.JsonCodec[$Tname] {
            ..$codecs
            $encoder
            $decoder
          }) : _root_.com.rojoma.json.codec.JsonCodec[$Tname]"""

    c.Expr[JsonCodec[T]](tree)
  }
}
