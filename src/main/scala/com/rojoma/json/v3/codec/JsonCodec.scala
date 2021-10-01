package com.rojoma.json.v3.codec

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.util.{JsonCaseInsensitiveEnum, JsonEnumStrategy, Strategy}
import com.rojoma.json.v3.`-impl`

object JsonCodec {
  def scalaEnumCodec[T <: Enumeration](enum: T): JsonEncode[enum.Value] with JsonDecode[enum.Value] = {
    val isCaseInsensitive = enum.getClass.isAnnotationPresent(classOf[JsonCaseInsensitiveEnum])
    val isUnderscoreized = Option(enum.getClass.getAnnotation(classOf[JsonEnumStrategy])).
      fold(false) { ann =>
        ann.value() match {
          case Strategy.Underscore =>
            true
          case Strategy.Identity =>
            false
        }
      }

    if(isCaseInsensitive || isUnderscoreized) {
      val nameMap: Map[String, enum.Value] = enum.
        values.
        iterator.
        map { e =>
          val name =
            if(isUnderscoreized) `-impl`.util.CamelSplit(e.toString).mkString("_").toLowerCase.intern()
            else if(isCaseInsensitive) e.toString.toLowerCase.intern()
            else e.toString
          name -> e
        }.
        toMap

      val valueMap: Map[enum.Value, String] = enum.
        values.
        iterator.
        map { e =>
          val name =
            if(isUnderscoreized) `-impl`.util.CamelSplit(e.toString).mkString("_").toLowerCase.intern()
            else e.toString
          e -> name
        }.
        toMap

      if(isCaseInsensitive) {
        new JsonEncode[enum.Value] with JsonDecode[enum.Value] {
          def encode(x: enum.Value) = JString(valueMap(x))
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
        new JsonEncode[enum.Value] with JsonDecode[enum.Value] {
          def encode(x: enum.Value) = JString(valueMap(x))
          def decode(x: JValue) = x match {
            case JString(s) =>
              nameMap.get(s) match {
                case Some(e) => Right(e)
                case None => Left(DecodeError.InvalidValue(x))
              }
            case other =>
              Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
          }
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
}
