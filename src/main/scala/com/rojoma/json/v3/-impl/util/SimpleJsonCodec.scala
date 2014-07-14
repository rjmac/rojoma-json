package com.rojoma.json.v3
package `-impl`.util

import codec._

sealed abstract class JsonCodecOrOption[T]
object JsonCodecOrOption {
  implicit def jcVersion[T: JsonEncode: JsonDecode]: JsonCodecOrOption[T] =
    new JsonCodecVersion[T](JsonEncode[T], JsonDecode[T])
  implicit def oVersion[T: JsonEncode: JsonDecode]: JsonCodecOrOption[Option[T]] =
    new OptionVersion[T](JsonEncode[T], JsonDecode[T])
}
class JsonCodecVersion[T](val enc: JsonEncode[T], val dec: JsonDecode[T]) extends JsonCodecOrOption[T] {
  type RealType = T
}
class OptionVersion[T](val enc: JsonEncode[T], val dec: JsonDecode[T]) extends JsonCodecOrOption[Option[T]] {
  type RealType = T
}
