package com.rojoma.`json-impl`.util

import scala.collection.mutable.ListBuffer

import scala.reflect.macros.Context

import com.rojoma.json.codec.JsonCodec

object MagicCaseClassCodecBuilderImpl {
  def impl[T : c.WeakTypeTag](c: Context): c.Expr[JsonCodec[T]] = {
    import c.universe._

    val T = weakTypeOf[T]
    val Tname = T.typeSymbol.name

    /*
    println(T.members)
    for {
      member <- T.members
      if member.isMethod && member.asMethod.isCaseAccessor
    } {
      println(member + " is a case accessor!")
      val mem = member.asMethod
      println("  Annotations: " + mem.annotations)
    }
    */

    def containsLazyAnnotation(param: Symbol) = false
    def computeJsonName(param: Symbol) = "gnu"
    def findAccessor(param: Symbol) = c.freshName()
    def findCodecType(param: Symbol) = c.freshName()
    def isOption(param: Symbol) = false
    def containsNullForNameAnnotation(param: Symbol) = false

    case class FieldInfo(codecName: TermName, isLazy: Boolean, jsonName: String, accessorName: TermName, codecType: TypeName, isOption: Boolean, isNullForNone: Boolean)

    val fieldss = locally {
      val buffer = new ListBuffer[List[FieldInfo]]
      for {
        member <- T.members
        if member.isMethod && member.asMethod.isPrimaryConstructor
      } {
        println(member + " is the primary ctor!")
        val mem = member.asMethod
        if(mem.owner == T.typeSymbol) {
          for {
            params <- mem.paramss
          } {
            val fieldList =
              for { param <- params }
              yield {
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
            buffer += fieldList
          }
        }
      }
      buffer.toList
    }
    val fields = fieldss.flatten

    val codecs = fields.map { fi =>
      // TODO: figure out how to add the "lazy" modifier after the fact
      if(fi.isLazy) {
        q"lazy val ${fi.codecName} = implicitly[com.rojoma.json.codec.JsonCodec[${fi.codecType}]]"
      } else {
        q"val ${fi.codecName} = implicitly[com.rojoma.json.codec.JsonCodec[${fi.codecType}]]"
      }
    }

    val param: TermName = c.freshName()
    val encoderMap: TermName = c.freshName()
    val encoderMapUpdates = for(fi <- fields) yield {
      if(fi.isOption) {
        val tmp: TermName = c.freshName()
        if(fi.isNullForNone) {
          q"""${encoderMap}(${fi.jsonName}) = ${param}.${fi.accessorName} match {
                case Some(${tmp}) => ${fi.codecName}.encode(${tmp})
                case None => com.rojoma.json.JNull
              }"""
        } else {
          q"""${param}.${fi.accessorName} match {
                case Some(${tmp}) => ${encoderMap}(${fi.jsonName}) = ${fi.codecName}.encode(${tmp})
                case None => /* pass */
              }"""
        }
      } else {
        q"${encoderMap}(${fi.jsonName}) = ${fi.codecName}.encode(${param}.${fi.accessorName})"
      }
    }
    val encoder = q"""def encoder(${param}: ${Tname}) = {
                        val ${encoderMap} = new scala.collection.mutable.LinkedHashMap[String, com.rojoma.json.JValue]
                        ..${encoderMapUpdates}
                        com.rojoma.json.JObject(${encoderMap})
                      }"""

    val obj: TermName = c.freshName()
    val tmps = fieldss.map { _.map { _ => TermName(c.freshName()) } }
    val decoderMapExtractions: List[ValDef] = for((fi,tmp) <- fields.zip(tmps.flatten)) yield {
      val tmp2: TermName = c.freshName()
      val value: TermName = c.freshName()
      if(fi.isOption) {
        val tmp3: TermName = c.freshName()
        q"""val $tmp = ${obj}.get(${fi.jsonName}) match {
              case Some($tmp2) =>
                ${fi.codecName}.decode($tmp2) match {
                  case Some($tmp3) => Some($tmp3)
                  case None => if($tmp2 == com.rojoma.json.ast.JNull) None else return None
                }
              case None =>
                None
            }"""
      } else {
        q"""val $tmp = ${obj}.get(${fi.jsonName}) match {
              case Some($tmp2) =>
                ${fi.codecName}.decode($tmp2) match {
                  case Some(value) => value
                  case None => return None
                }
              case None => return None
            }"""
      }
    }
    val create = q"new ${Tname}(${param},${param},${param})"
    val c2 = q"new Blah(a,b)(c,d)"
    println(c2);
    val decoder = q"""def decoder(${param}: com.rojoma.json.ast.JValue): Option[${Tname}] = ${param} match {
                        case com.rojoma.json.ast.JObject(${obj}) =>
                          ..${decoderMapExtractions}
                          $create
                        case _ => None
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
      q"""new com.rojoma.json.codec.JsonCodec[${Tname}] {
            ..${codecs}
            ${encoder}
            ${decoder}
          }"""

    println(tree)

    c.Expr[JsonCodec[T]](tree)
  }
}
