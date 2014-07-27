package com.rojoma.json.v3
package dynamic

import scala.language.dynamics
import scala.{collection => sc}

import `-impl`.dynamic._
import ast._
import codec.DecodeError
import codec.Path

class BadPath(val error: DecodeError.Simple) extends NoSuchElementException(error.english)

sealed trait InformationalDynamicJValue extends Dynamic {
  def ? : Either[DecodeError.Simple, JValue]
  def ! : JValue

  def apply(idx: Int): InformationalDynamicJValue
  def apply(field: String): InformationalDynamicJValue

  def applyDynamic(field: String) = apply(field)
  def selectDynamic(field: String) = apply(field)
}

object InformationalDynamicJValue extends (JValue => InformationalDynamicJValue) {
  def apply(v: JValue): InformationalDynamicJValue = new Good(v, Nil)

  private class Bad(err: DecodeError.Simple) extends InformationalDynamicJValue {
    val ? = Left(err)
    def ! = throw new BadPath(err)

    def apply(idx: Int) = this
    def apply(field: String) = this
  }

  private class Good(val ! : JValue, path: List[Path.Entry]) extends InformationalDynamicJValue {
    def ? = Right(this.!)

    def apply(idx: Int) =
      this.! match {
        case JArray(arr) if arr.isDefinedAt(idx) =>
          new Good(arr(idx), Path.Index(idx) :: path)
        case JArray(arr) =>
          new Bad(DecodeError.InvalidLength(expected = idx + 1, got = arr.length, path = new Path(path.reverse)))
        case other =>
          new Bad(DecodeError.InvalidType(expected = JArray, got = other.jsonType, path = new Path(path.reverse)))
      }

    def apply(field: String) =
      this.! match {
        case JObject(obj) =>
          obj.get(field) match {
            case Some(result) =>
              new Good(result, Path.Field(field) :: path)
            case None =>
              new Bad(DecodeError.MissingField(field, new Path(path.reverse)))
          }
        case other =>
          new Bad(DecodeError.InvalidType(expected = JObject, got = other.jsonType, path = new Path(path.reverse)))
      }
  }
}

@deprecated(message = "Prefer `InformationalDynamicJValue`", since = "3.1.1")
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
