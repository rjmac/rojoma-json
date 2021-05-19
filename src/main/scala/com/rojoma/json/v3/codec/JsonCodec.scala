package com.rojoma.json.v3.codec

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.util.JsonCaseInsensitiveEnum

object JsonCodec {
  def scalaEnumCodec[T <: Enumeration](e: T): JsonEncode[e.Value] with JsonDecode[e.Value] =
    if(e.getClass.isAnnotationPresent(classOf[JsonCaseInsensitiveEnum])) {
      new JsonEncode[e.Value] with JsonDecode[e.Value] {
        val nameMap = e.
          values.
          iterator.
          map { e => e.toString.toLowerCase -> e }.
          toMap

        def encode(x: e.Value) = JString(x.toString)
        def decode(x: JValue) = x match {
          case JString(s) =>
            nameMap.get(s.toLowerCase) match {
              case Some(e) => Right(e)
              case None => Left(DecodeError.InvalidValue(x))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
      }
    } else {
      new JsonEncode[e.Value] with JsonDecode[e.Value] {
        def encode(x: e.Value) = JString(x.toString)
        def decode(x: JValue) = x match {
          case JString(s) =>
            try { Right(e.withName(s)) }
            catch { case _: NoSuchElementException => Left(DecodeError.InvalidValue(x)) }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
      }
    }
}
