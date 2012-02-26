package com.rojoma.`json-impl`.util

import com.rojoma.json.codec._

sealed abstract class JsonCodecOrOption[T]
object JsonCodecOrOption {
  implicit def jcVersion[T: JsonCodec]: JsonCodecOrOption[T] = new JsonCodecVersion[T](JsonCodec[T])
  implicit def oVersion[T: JsonCodec]: JsonCodecOrOption[Option[T]] = new OptionVersion[T](JsonCodec[T])
}
class JsonCodecVersion[T](val codec: JsonCodec[T]) extends JsonCodecOrOption[T] {
  type RealType = T
}
class OptionVersion[T](val codec: JsonCodec[T]) extends JsonCodecOrOption[Option[T]] {
  type RealType = T
}
