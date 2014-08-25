package com.rojoma.json.v3
package `-impl`.conversions.v2

import scala.language.implicitConversions

import com.rojoma.json.{ast => v2ast}
import com.rojoma.json.v3.{ast => v3ast}
import com.rojoma.json.{io => v2io}
import com.rojoma.json.v3.{io => v3io}
import com.rojoma.json.{codec => v2codec}
import com.rojoma.json.v3.{codec => v3codec}

class Conversions extends LowPriorityConversions {
  implicit def toNumberConversion(x: v3ast.JNumber) = new ToV2Number(x)
  implicit def toStringConversion(x: v3ast.JString) = new ToV2String(x)
  implicit def toBooleanConversion(x: v3ast.JBoolean) = new ToV2Boolean(x)
  implicit def toNullConversion(x: v3ast.JNull) = new ToV2Null(x)
  implicit def toObjectConversion(x: v3ast.JObject) = new ToV2Object(x)
  implicit def toArrayConversion(x: v3ast.JArray) = new ToV2Array(x)

  implicit def fromNumberConversion(x: v2ast.JNumber) = new FromV2Number(x)
  implicit def fromStringConversion(x: v2ast.JString) = new FromV2String(x)
  implicit def fromBooleanConversion(x: v2ast.JBoolean) = new FromV2Boolean(x)
  implicit def fromNullConversion(x: v2ast.JNull) = new FromV2Null(x)
  implicit def fromObjectConversion(x: v2ast.JObject) = new FromV2Object(x)
  implicit def fromArrayConversion(x: v2ast.JArray) = new FromV2Array(x)

  // Unlike JValues, it is unlikely to be worth preserving precise
  // types for tokens and events.  Certainly not in the iterator case!
  // But just in case, the implicit conversions live in LowPriorityConversion
  // for binary compat reasons.

  /** (Implicitly) create a JsonCodec from an encode/decode */
  implicit def jsonCodec[T : v3codec.JsonEncode : v3codec.JsonDecode] = new v2codec.JsonCodec[T] {
    def encode(x: T) = v3codec.JsonEncode.toJValue(x).toV2
    def decode(x: v2ast.JValue) = v3codec.JsonDecode.fromJValue(x.toV3).right.toOption
  }

  /** Creates a JsonCodec that can only encode */
  def partialEncode[T : v3codec.JsonEncode] = new v2codec.JsonCodec[T] {
    def encode(x: T) = v3codec.JsonEncode.toJValue(x).toV2
    def decode(x: v2ast.JValue) = sys.error("partialEncode.decode")
  }

  /** Creates a JsonCodec that can only decode */
  def partialDecode[T : v3codec.JsonDecode] = new v2codec.JsonCodec[T] {
    def encode(x: T) = sys.error("partialDecode.encode")
    def decode(x: v2ast.JValue) = v3codec.JsonDecode.fromJValue(x.toV3).right.toOption
  }
}
