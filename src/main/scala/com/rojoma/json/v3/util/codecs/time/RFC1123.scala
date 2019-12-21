package com.rojoma.json.v3.util.codecs.time

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.codec._

/** Codecs that serialize times as RFC 1123 values.  Note that these
  * codecs are lossy, and indeed cannot even encode dates that can't
  * be expressed with a four-digit positive year!  They're provided
  * for compatibility with other systems that produce or consume such
  * timestamps.  If you have the choice, use the ISO8601 codecs
  * instead.
  */
object RFC1123 {
  implicit object instant extends JsonEncode[Instant] with JsonDecode[Instant] {
    def encode(x: Instant) =
      JString(DateTimeFormatter.RFC_1123_DATE_TIME.format(x.atOffset(ZoneOffset.UTC)))

    def decode(x: JValue) =
      x match {
        case jstr@JString(s) =>
          try {
            Right(DateTimeFormatter.RFC_1123_DATE_TIME.parse(s, Instant.from))
          } catch {
            case e: DateTimeParseException =>
              Left(DecodeError.InvalidValue(jstr))
          }
        case other =>
          Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
      }
  }

  implicit object offsetDateTime extends JsonEncode[OffsetDateTime] with JsonDecode[OffsetDateTime] {
    def encode(x: OffsetDateTime) =
      JString(DateTimeFormatter.RFC_1123_DATE_TIME.format(x))

    def decode(x: JValue) =
      x match {
        case jstr@JString(s) =>
          try {
            Right(DateTimeFormatter.RFC_1123_DATE_TIME.parse(s, OffsetDateTime.from))
          } catch {
            case e: DateTimeParseException =>
              Left(DecodeError.InvalidValue(jstr))
          }
        case other =>
          Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
      }
  }
}
