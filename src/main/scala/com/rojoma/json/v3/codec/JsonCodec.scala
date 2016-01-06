package com.rojoma.json.v3.codec

import com.rojoma.json.v3.ast._

object JsonCodec {
  def scalaEnumCodec[T <: Enumeration](enum: T): JsonEncode[enum.Value] with JsonDecode[enum.Value] =
    new JsonEncode[enum.Value] with JsonDecode[enum.Value] {
      def encode(x: enum.Value) = JString(x.toString)
      def decode(x: JValue) = x match {
        case JString(s) =>
          try { Right(enum.withName(s)) }
          catch { case _: NoSuchElementException => Left(DecodeError.InvalidValue(x)) }
        case other =>
          Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
      }
    }
}
