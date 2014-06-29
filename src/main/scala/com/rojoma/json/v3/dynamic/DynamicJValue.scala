package com.rojoma.json.v3
package dynamic

import scala.language.dynamics
import scala.{collection => sc}

import `-impl`.dynamic._
import ast._

class DynamicJValue(val ? : Option[JValue]) extends AnyVal with Dynamic {
  def ! =
    ? match {
      case Some(x) => x
      case None => throw new NoSuchElementException("DynamicJValue.static")
    }

  def applyDynamic[T](field: String)(subfieldOrIdx: T)(implicit ev: DynamicPathType[T]): DynamicJValue =
    ? match {
      case Some(JObject(fields)) =>
        fields.get(field) match {
          case Some(item) =>
            item match {
              case obj: JObject if ev.isField =>
                obj.dynamic(ev.asField(subfieldOrIdx))
              case arr: JArray if !ev.isField =>
                arr.dynamic(ev.asIndex(subfieldOrIdx))
              case _=>
                new DynamicJValue(None)
            }
          case None =>
            new DynamicJValue(None)
        }
      case _ =>
        new DynamicJValue(None)
    }

  def selectDynamic(field: String): DynamicJValue =
    apply(field)

  def apply(idx: Int): DynamicJValue =
    ? match {
      case Some(arr: JArray) if arr.isDefinedAt(idx) =>
        arr(idx).dynamic
      case _ =>
        new DynamicJValue(None)
    }

  def apply(field: String): DynamicJValue =
    ? match {
      case Some(obj: JObject) =>
        new DynamicJValue(obj.get(field))
      case _ =>
        new DynamicJValue(None)
    }
}
