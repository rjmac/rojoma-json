package com.rojoma.json.v3
package `-impl`.util

import codec._

sealed abstract class JsonCodecOrOption[T]
object JsonCodecOrOption {
  implicit def jcVersion[T: JsonEncode: JsonDecode]: JsonCodecOrOption[T] =
    JsonCodecVersion[T](JsonEncode[T], JsonDecode[T])
  implicit def oVersion[T: JsonEncode: JsonDecode]: JsonCodecOrOption[Option[T]] =
    OptionVersion[T](JsonEncode[T], JsonDecode[T])
}
case class JsonCodecVersion[T](enc: JsonEncode[T], dec: JsonDecode[T]) extends JsonCodecOrOption[T] {
  type RealType = T
}
case class OptionVersion[T](enc: JsonEncode[T], dec: JsonDecode[T]) extends JsonCodecOrOption[Option[T]] {
  type RealType = T
}
