package com.rojoma.json.v3.util.time

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.Date

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.codec._

/** Codecs that serialize times as RFC 1123 values.  Note that these
  * codecs are lossy, and indeed cannot even encode dates that can't
  * be expressed with a four-digit positive year!  They're provided
  * for compatibility with other systems that produce or consume such
  * timestamps.  If you have the choice, use the ISO8601 codecs
  * instead.
  */
package RFC1123 {
  object codec {
    given dateCodec: JsonEncode[Date] with JsonDecode[Date] with {
      def encode(x: Date) = instantCodec.encode(x.toInstant)
      def decode(x: JValue) = instantCodec.decode(x).flatMap { instant =>
        try {
          Right(new Date(instant.toEpochMilli))
        } catch {
          case e: ArithmeticException =>
            Left(DecodeError.InvalidValue(x))
        }
      }
    }

    given instantCodec: JsonEncode[Instant] with JsonDecode[Instant] with {
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

    given offsetDateTimeCodec: JsonEncode[OffsetDateTime] with JsonDecode[OffsetDateTime] with {
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

  object encode {
    given instantEncode : JsonEncode[Instant] = codec.instantCodec
    given offsetDateTimeEncode : JsonEncode[OffsetDateTime] = codec.offsetDateTimeCodec
  }

  object decode {
    given instantDecode : JsonDecode[Instant] = codec.instantCodec
    given offsetDateTimeDecode : JsonDecode[OffsetDateTime] = codec.offsetDateTimeCodec
  }
}
