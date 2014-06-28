package com.rojoma.json.v3
package dynamic

import scala.language.dynamics
import scala.{collection => sc}

import `-impl`.dynamic._
import ast._

class DynamicJValue(val staticOpt: Option[JValue]) extends AnyVal with Dynamic {
  def static =
    staticOpt match {
      case Some(x) => x
      case None => throw new NoSuchElementException("DynamicJValue.static")
    }

  def applyDynamic[T](field: String)(subfieldOrIdx: T)(implicit ev: DynamicPathType[T]): DynamicJValue =
    staticOpt match {
      case Some(JObject(fields)) =>
        fields.get(field) match {
          case Some(item) =>
            item match {
              case obj: JObject if ev.isField =>
                obj.dynamic.selectDynamic(ev.asField(subfieldOrIdx))
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
    staticOpt match {
      case Some(obj: JObject) =>
        new DynamicJValue(obj.get(field))
      case _ =>
        new DynamicJValue(None)
    }

  def apply(idx: Int): DynamicJValue =
    staticOpt match {
      case Some(arr: JArray) if arr.isDefinedAt(idx) =>
        arr(idx).dynamic
      case _ =>
        new DynamicJValue(None)
    }
}
