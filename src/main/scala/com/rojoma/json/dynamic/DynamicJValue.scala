package com.rojoma.json
package dynamic

import scala.language.dynamics

import ast._

import com.rojoma.`json-impl`.dynamic._

class DynamicJValue(val static: JValue) extends AnyVal with Dynamic {
  def applyDynamic(field: String)(subfieldOrIdx: DynamicDisambiguate): DynamicJValue =
    static match {
      case JObject(fields) =>
        subfieldOrIdx match {
          case Field(subfield) =>
            fields(field) match {
              case JObject(subfields) => subfields(subfield).dynamic
              case _ => throw new InvalidDynamicJValueTypeException("Not an object")
            }
          case Index(elem) =>
            fields(field) match {
              case JArray(elems) => elems(elem).dynamic
              case _ => throw new InvalidDynamicJValueTypeException("Not an array")
            }
        }
      case _ => throw new InvalidDynamicJValueTypeException("Not an object")
    }

  def selectDynamic(field: String): DynamicJValue =
    static match {
      case JObject(fields) => fields(field).dynamic
      case _ => throw new InvalidDynamicJValueTypeException("Not an object")
    }

  def apply(idx: Int): DynamicJValue =
    static match {
      case JArray(elements) => elements(idx).dynamic
      case _ => throw new InvalidDynamicJValueTypeException("Not an array")
    }

  override def toString = static.toString
}

object DynamicJValue {
  def apply(v: JValue) = new DynamicJValue(v)
}
