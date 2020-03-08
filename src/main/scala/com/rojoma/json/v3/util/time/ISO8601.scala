package com.rojoma.json.v3.util.time

import java.util.regex.Pattern
import java.util.Date
import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.codec._

package ISO8601 {
  object codec {
    // technically this is mixing basic and extended timestamps, which
    // is not ISO, but it's common enough that I'll allow it.
    private val offsetDateTimeFixupPattern = Pattern.compile("^(.*[tT].*[+-])([0-9]{2}(?:[0-9]{2})?)$")

    private def fixupOffset(pattern: Pattern, s: String): String = {
      val m = pattern.matcher(s)
      if(m.matches) {
        val base = m.group(1)
        val tz = m.group(2)
        if(tz.length == 4) {
          s"$base${tz.substring(0,2)}:${tz.substring(2)}"
        } else {
          s"$base$tz:00"
        }
      } else {
        s
      }
    }

    implicit object dateCodec extends JsonEncode[Date] with JsonDecode[Date] {
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

    implicit object instantCodec extends JsonEncode[Instant] with JsonDecode[Instant] {
      private val fixupPattern = offsetDateTimeFixupPattern

      def encode(x: Instant) =
        JString(DateTimeFormatter.ISO_INSTANT.format(x))

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(fixupOffset(fixupPattern, s), Instant.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object offsetDateTimeCodec extends JsonEncode[OffsetDateTime] with JsonDecode[OffsetDateTime] {
      private val fixupPattern = offsetDateTimeFixupPattern

      def encode(x: OffsetDateTime) =
        JString(DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(x))

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(fixupOffset(fixupPattern, s), OffsetDateTime.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object offsetTimeCodec extends JsonEncode[OffsetTime] with JsonDecode[OffsetTime] {
      def encode(x: OffsetTime) =
        JString(DateTimeFormatter.ISO_OFFSET_TIME.format(x))

      private val fixupPattern = Pattern.compile("^(.*[+-])([0-9]{2}(?:[0-9]{2})?)$")

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_OFFSET_TIME.parse(fixupOffset(fixupPattern, s), OffsetTime.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object zoneOffsetCodec extends JsonEncode[ZoneOffset] with JsonDecode[ZoneOffset] {
      def encode(x: ZoneOffset) = JString(x.toString)

      private val fixupPattern = Pattern.compile("^([+-])([0-9]{2}(?:[0-9]{2})?)$")

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(ZoneOffset.of(fixupOffset(fixupPattern, s)))
            } catch {
              case e: DateTimeException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object localDateTimeCodec extends JsonEncode[LocalDateTime] with JsonDecode[LocalDateTime] {
      def encode(x: LocalDateTime) =
        JString(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(x))

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse(s, LocalDateTime.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object localDateCodec extends JsonEncode[LocalDate] with JsonDecode[LocalDate] {
      def encode(x: LocalDate) =
        JString(DateTimeFormatter.ISO_LOCAL_DATE.format(x))

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_LOCAL_DATE.parse(s, LocalDate.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object localTimeCodec extends JsonEncode[LocalTime] with JsonDecode[LocalTime] {
      def encode(x: LocalTime) =
        JString(DateTimeFormatter.ISO_LOCAL_TIME.format(x))

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(DateTimeFormatter.ISO_LOCAL_TIME.parse(s, LocalTime.from))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object yearMonthCodec extends JsonEncode[YearMonth] with JsonDecode[YearMonth] {
      def encode(x: YearMonth) = {
        val s = x.toString
        if(x.getYear > 9999 && s.charAt(0) != '+') JString("+" + s)
        else JString(s)
      }

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(YearMonth.parse(s))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object monthDayCodec extends JsonEncode[MonthDay] with JsonDecode[MonthDay] {
      def encode(x: MonthDay) = JString(x.toString)

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(MonthDay.parse(s))
            } catch {
              case e: DateTimeParseException =>
                Left(DecodeError.InvalidValue(jstr))
            }
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }
    }

    implicit object durationCodec extends JsonEncode[Duration] with JsonDecode[Duration] {
      // Due to a bug in Java 8 (https://bugs.openjdk.java.net/browse/JDK-8054978) this
      // actually doesn't work quite properly!
      val jdkDurationBug = Duration.parse("PT-0.5S").toString == "PT0.5S"

      private val problematicPattern = Pattern.compile("""^([^S]*[A-RT-Z])-0+\.([0-9]{1,9})*S$""", Pattern.CASE_INSENSITIVE)

      def encode(x: Duration) = JString(x.toString)

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            if(jdkDurationBug) safePath(s, jstr)
            else nativePath(s, jstr)
          case other =>
            Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
        }

      private def safePath(s: String, jstr: JString): Either[DecodeError, Duration] = {
        try {
          val m = problematicPattern.matcher(s)
          if(m.matches) {
            val goodPrefix = m.group(1)
            val badSuffixNanoStr = m.group(2)
            var scale = 1L
            var expt = 9-badSuffixNanoStr.length
            while(expt > 0) { scale *= 10; expt -= 1 }
            val badSuffixNanos = badSuffixNanoStr.toLong * scale
            if(goodPrefix.equalsIgnoreCase("PT")) {
              Right(Duration.ZERO.minusNanos(badSuffixNanos))
            } else {
              Right(Duration.parse(goodPrefix).minusNanos(badSuffixNanos))
            }
          } else {
            Right(Duration.parse(s))
          }
        } catch {
          case _ : ArithmeticException | _ : DateTimeParseException =>
            Left(DecodeError.InvalidValue(jstr))
        }
      }

      private def nativePath(s: String, jstr: JString): Either[DecodeError, Duration] = {
        try {
          Right(Duration.parse(s))
        } catch {
          case e: DateTimeParseException =>
            Left(DecodeError.InvalidValue(jstr))
        }
      }
    }

    implicit object periodCodec extends JsonEncode[Period] with JsonDecode[Period] {
      def encode(x: Period) = JString(x.toString)

      def decode(x: JValue) =
        x match {
          case jstr@JString(s) =>
            try {
              Right(Period.parse(s))
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
    implicit val dateEncode : JsonEncode[Date] = codec.dateCodec
    implicit val instantEncode : JsonEncode[Instant] = codec.instantCodec
    implicit val offsetDateTimeEncode : JsonEncode[OffsetDateTime] = codec.offsetDateTimeCodec
    implicit val offsetTimeEncode : JsonEncode[OffsetTime] = codec.offsetTimeCodec
    implicit val zoneOffsetEncode : JsonEncode[ZoneOffset] = codec.zoneOffsetCodec
    implicit val localDateTimeEncode : JsonEncode[LocalDateTime] = codec.localDateTimeCodec
    implicit val localDateEncode : JsonEncode[LocalDate] = codec.localDateCodec
    implicit val localTimeEncode : JsonEncode[LocalTime] = codec.localTimeCodec
    implicit val yearMonthEncode : JsonEncode[YearMonth] = codec.yearMonthCodec
    implicit val monthDayEncode : JsonEncode[MonthDay] = codec.monthDayCodec
    implicit val durationEncode : JsonEncode[Duration] = codec.durationCodec
    implicit val periodEncode : JsonEncode[Period] = codec.periodCodec
  }

  object decode {
    implicit val dateDecode : JsonDecode[Date] = codec.dateCodec
    implicit val instantDecode : JsonDecode[Instant] = codec.instantCodec
    implicit val offsetDateTimeDecode : JsonDecode[OffsetDateTime] = codec.offsetDateTimeCodec
    implicit val offsetTimeDecode : JsonDecode[OffsetTime] = codec.offsetTimeCodec
    implicit val zoneOffsetDecode : JsonDecode[ZoneOffset] = codec.zoneOffsetCodec
    implicit val localDateTimeDecode : JsonDecode[LocalDateTime] = codec.localDateTimeCodec
    implicit val localDateDecode : JsonDecode[LocalDate] = codec.localDateCodec
    implicit val localTimeDecode : JsonDecode[LocalTime] = codec.localTimeCodec
    implicit val yearMonthDecode : JsonDecode[YearMonth] = codec.yearMonthCodec
    implicit val monthDayDecode : JsonDecode[MonthDay] = codec.monthDayCodec
    implicit val durationDecode : JsonDecode[Duration] = codec.durationCodec
    implicit val periodDecode : JsonDecode[Period] = codec.periodCodec
  }
}
