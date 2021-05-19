package com.rojoma.json.v3
package dynamic

import scala.language.dynamics

import ast._
import codec.DecodeError
import zipper.{JsonZipper, JArrayZipper, JObjectZipper}

class BadPath(val error: DecodeError.Simple) extends NoSuchElementException(error.english)

sealed trait InformationalDynamicJValue extends Dynamic {
  def ? : Either[DecodeError.Simple, JValue]
  def ! : JValue
  def ^? : Either[DecodeError.Simple, JsonZipper]
  def ^! : JsonZipper

  def apply(idx: Int): InformationalDynamicJValue
  def apply(field: String): InformationalDynamicJValue

  def applyDynamic(field: String) = apply(field)
  def selectDynamic(field: String) = apply(field)
}

object InformationalDynamicJValue extends (JValue => InformationalDynamicJValue) {
  def apply(v: JValue): InformationalDynamicJValue = new Good(JsonZipper(v))

  private class Bad(err: DecodeError.Simple) extends InformationalDynamicJValue {
    def ? = Left(err)
    def ! = throw new BadPath(err)
    def ^? = Left(err)
    def ^! = throw new BadPath(err)

    def apply(idx: Int) = this
    def apply(field: String) = this
  }

  private class Good(zipper : JsonZipper) extends InformationalDynamicJValue {
    def ! = zipper.value : JValue
    def ? = Right(this.!)
    def ^? = Right(zipper)
    def ^! = zipper

    def apply(idx: Int) =
      zipper match {
        case arr: JArrayZipper if arr.value.isDefinedAt(idx) =>
          new Good(arr.down_!(idx))
        case arr: JArrayZipper =>
          new Bad(DecodeError.InvalidLength(expected = idx + 1, got = arr.value.length, path = arr.path))
        case other =>
          new Bad(DecodeError.InvalidType(expected = JArray, got = other.value.jsonType, path = other.path))
      }

    def apply(field: String) =
      zipper match {
        case obj: JObjectZipper =>
          obj.down(field) match {
            case Some(result) =>
              new Good(result)
            case None =>
              new Bad(DecodeError.MissingField(field, zipper.path))
          }
        case other =>
          new Bad(DecodeError.InvalidType(expected = JObject, got = other.value.jsonType, path = other.path))
      }
  }
}
