package com.rojoma.json.v3.codec

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.util.JsonCaseInsensitiveEnum

object JsonCodec {
  def scalaEnumCodec[T <: Enumeration](enum: T): JsonEncode[enum.Value] with JsonDecode[enum.Value] =
    if(enum.getClass.isAnnotationPresent(classOf[JsonCaseInsensitiveEnum])) {
      new JsonEncode[enum.Value] with JsonDecode[enum.Value] {
        val nameMap = enum.
          values.
          iterator.
          map { e ⇒ e.toString.toLowerCase → e }.
          toMap

        def encode(x: enum.Value) = JString(x.toString)
        def decode(x: JValue) = x match {
          case JString(s) =>
            nameMap.get(s.toLowerCase) match {
              case Some(e) ⇒ Right(e)
              case None ⇒ Left(DecodeError.InvalidValue(x))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
      }
    } else {
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
}
