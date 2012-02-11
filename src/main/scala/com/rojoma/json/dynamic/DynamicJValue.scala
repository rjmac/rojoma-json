package com.rojoma.json
package dynamic

import ast._

import com.rojoma.`json-impl`.dynamic._

class DynamicJValue(val static: JValue) extends BaseClassHolder.BaseClass with Dynamic {
  def applyDynamic(field: String)(arg: DynamicDisambiguate = NotProvided): DynamicJValue = {
    val a = apply(field)
    arg match {
      case NotProvided => a
      case Index(idx) => a(idx)
      case Field(subfield) => a(subfield)
    }
  }

   /** Allow names that collide with names the Scala compiler recognizes
     * to be used with a minimum of boilerplate. */
   def apply(field: String): DynamicJValue =
    static match {
      case JObject(fields) => fields(field).dynamic
      case _ => throw new InvalidDynamicJValueTypeException("Not an object")
    }

  def apply(idx: Int): DynamicJValue =
    static match {
      case JArray(elements) => elements(idx).dynamic
      case _ => throw new InvalidDynamicJValueTypeException("Not an array")
    }
}

object DynamicJValue {
  def apply(v: JValue) = new DynamicJValue(v)
}
