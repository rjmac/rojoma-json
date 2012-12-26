package com.rojoma.`json-impl`.util

import scala.collection.mutable.ListBuffer

import scala.reflect.macros.Context

import com.rojoma.json.codec.JsonCodec
import com.rojoma.json.util.{JsonName, LazyCodec, NullForNone}

object MagicCaseClassCodecBuilderImpl {
  def impl[T : c.WeakTypeTag](c: Context): c.Expr[JsonCodec[T]] = {
    import c.universe._

    val T = weakTypeOf[T]
    val Tname = TypeTree(T)

    def containsLazyAnnotation(param: TermSymbol) =
      param.annotations.exists(_.tpe == typeOf[LazyCodec])

    def computeJsonName(param: TermSymbol): String = {
      var name = param.name.decoded
      for(ann <- param.annotations) {
        if(ann.tpe == typeOf[JsonName]) {
          ann.javaArgs(TermName("value")) match {
            case LiteralArgument(Constant(s: String)) =>
              name = s
          }
          println("Name! " + ann)
        }
      }
      name
    }
    def findAccessor(param: TermSymbol) =
      param.name.asInstanceOf[TermName]

    def findCodecType(param: TermSymbol) = {
      val tpe = param.typeSignature
      if(tpe.erasure =:= typeOf[Option[_]].erasure) {
        val TypeRef(_, _, c) = tpe
        c.head
      } else {
        tpe
      }.asSeenFrom(T, T.typeSymbol)
    }
    def isOption(param: TermSymbol) = {
      val tpe = param.typeSignature
      tpe.erasure =:= typeOf[Option[_]].erasure
    }
    def containsNullForNameAnnotation(param: TermSymbol) =
      param.annotations.exists(_.tpe == typeOf[NullForNone])

    case class FieldInfo(codecName: TermName, isLazy: Boolean, jsonName: String, accessorName: TermName, codecType: Type, isOption: Boolean, isNullForNone: Boolean)

    val fieldss = locally {
      val buffer = new ListBuffer[List[FieldInfo]]
      for {
        member <- T.members
        if member.isMethod && member.asMethod.isPrimaryConstructor
      } {
        println(member + " is the primary ctor!")
        val mem = member.asMethod
        mem.asInstanceOf[MethodSymbol]
        if(mem.owner == T.typeSymbol) {
          for {
            params <- mem.paramss
          } {
            var isImplicitList = false // a little icky, but meh...
            val fieldList =
              for { rawParam <- params }
              yield {
                val param = rawParam.asTerm
                if(param.isImplicit) isImplicitList = true
                FieldInfo(
                  c.freshName(),
                  containsLazyAnnotation(param),
                  computeJsonName(param),
                  findAccessor(param),
                  findCodecType(param),
                  isOption(param),
                  containsNullForNameAnnotation(param)
                )
              }
            if(!isImplicitList) buffer += fieldList
          }
        }
      }
      buffer.toList
    }
    val fields = fieldss.flatten

    val codecs = fields.map { fi =>
      // TODO: figure out how to add the "lazy" modifier after the fact
      if(fi.isLazy) {
        q"lazy val ${fi.codecName} = _root_.scala.Predef.implicitly[_root_.com.rojoma.json.codec.JsonCodec[${TypeTree(fi.codecType)}]]"
      } else {
        q"val ${fi.codecName} = _root_.scala.Predef.implicitly[_root_.com.rojoma.json.codec.JsonCodec[${TypeTree(fi.codecType)}]]"
      }
    }

    val param: TermName = c.freshName()
    val encoderMap: TermName = c.freshName()
    val encoderMapUpdates = for(fi <- fields) yield {
      if(fi.isOption) {
        val tmp: TermName = c.freshName()
        if(fi.isNullForNone) {
          q"""$encoderMap(${fi.jsonName}) = {
                // Hm, doesn't look like q can generate a match statement with holes yet
                // (it looks like it always tries to use the variables as constants)
                val $tmp = $param.${fi.accessorName}
                if($tmp.isDefined) ${fi.codecName}.encode($tmp.get)
                else com.rojoma.json.ast.JNull
              }"""
        } else {
          q"""{
                val $tmp = $param.${fi.accessorName}
                if($tmp.isDefined) $encoderMap(${fi.jsonName}) = ${fi.codecName}.encode($tmp.get)
              }"""
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
    val tmp2: TermName = c.freshName()
    val tmp3: TermName = c.freshName()
    val decoderMapExtractions: List[ValDef] = for((fi,tmp) <- fields.zip(tmps.flatten)) yield {
      if(fi.isOption) {
        q"""val $tmp = {
              val $tmp2 = $obj.get(${fi.jsonName})
              if($tmp2.isDefined) {
                val $tmp3 = ${fi.codecName}.decode($tmp2.get)
                if($tmp3.isDefined) $tmp3
                else if($tmp2.get == _root_.com.rojoma.json.ast.JNull) _root_.scala.None else return _root_.scala.None
              } else _root_.scala.None
            }"""
      } else {
        q"""val $tmp = {
              val $tmp2 = ${obj}.get(${fi.jsonName})
              if($tmp2.isDefined) {
                val $tmp3 = ${fi.codecName}.decode($tmp2.get)
                if($tmp3.isDefined) $tmp3.get
                else return _root_.scala.None
              } else return _root_.scala.None
            }"""
      }
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

    // Ok, what we need:
    //  1. Find the primary ctor and its parameters and their annotations
    //  2. Create local (non-implicit, field) references to the appropriate
    //     sub-Codecs.  TODO: recursive class hierarchies.  These can obviously
    //     be broken by making the fields "lazy vals" but that's an expensive
    //     thing for a rare case... maybe require a @LazyCodec annotation on
    //     the parameter for that.
    //
    //     ...actually, it should use the Pattern machinery, just like the
    //     SimpleJsonCodecBuilder.
    //  2. The encoder should build a map from either the Scala name or, if
    //     @JsonName is present on the constructor parameter, from that value.
    //     If the value is of type Option[T] it should eliminate `None`s unless
    //     @NullForNone is set on the parameter.
    //  3. The decoder should do the reverse.
    //     To think about: parameters with default values.  It sorta feels like
    //     they should be allowed to be missing in the JSON, but unfortunately
    //     I think that'd lead to a combinatorial explosion of code-paths in
    //     the decoder...

    val tree =
      q"""new _root_.com.rojoma.json.codec.JsonCodec[$Tname] {
            ..$codecs
            $encoder
            $decoder
          }"""

    println(tree)

    c.Expr[JsonCodec[T]](tree)
  }
}
