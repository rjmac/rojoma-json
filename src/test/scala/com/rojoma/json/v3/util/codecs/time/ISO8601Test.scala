package com.rojoma.json.v3.util.codecs.time

import java.time._
import java.util.Date

import org.scalatest.{FunSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.rojoma.json.v3.ast.JString
import com.rojoma.json.v3.codec.{JsonDecode, JsonEncode, DecodeError}

class ISO8601Test extends FunSpec with MustMatchers with ScalaCheckPropertyChecks {
  import ISO8601._

  describe("Date codec") {
    it("roundtrips") {
      forAll { epochMillis: Long =>
        val d = new Date(epochMillis)
        JsonDecode.fromJValue[Date](JsonEncode.toJValue(d)) must equal (Right(d))
      }
    }

    it("doesn't throw if the date is outside Date's range") {
      JsonDecode.fromJValue[Date](JsonEncode.toJValue(Instant.MAX)) must equal (Left(DecodeError.InvalidValue(JString("+1000000000-12-31T23:59:59.999999999Z"))))
    }
  }

  describe("Instant codec") {
    it("roundtrips") {
      forAll { (t: Instant) =>
        JsonDecode.fromJValue[Instant](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }

    it("ends with Z") {
      forAll { (t: Instant) =>
        val JString(s) = JsonEncode.toJValue(t)
        s must endWith ("Z")
      }
    }

    it("reads timestamps with offsets") {
      forAll { (t: OffsetDateTime) =>
        JsonDecode.fromJValue[Instant](JsonEncode.toJValue(t)) must equal (Right(t.toInstant))
      }
    }

    it("can parse offsets without a colon") {
      JsonDecode.fromJValue[Instant](JString("1234-10-13T12:34:56-0730")) must equal (Right(OffsetDateTime.parse("1234-10-13T12:34:56-07:30").toInstant))
    }

    it("can parse offsets with only an hour") {
      JsonDecode.fromJValue[Instant](JString("1234-10-13T12:34:56-07")) must equal (Right(OffsetDateTime.parse("1234-10-13T12:34:56-07:00").toInstant))
    }
  }

  describe("OffsetDateTime codec") {
    it("roundtrips") {
      forAll { (t: OffsetDateTime) =>
        JsonDecode.fromJValue[OffsetDateTime](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }

    it("can parse an offset of z Z") {
      forAll { (t: OffsetDateTime) =>
        JsonDecode.fromJValue[OffsetDateTime](JsonEncode.toJValue(t.toInstant)) must equal (Right(t.withOffsetSameInstant(ZoneOffset.UTC)))
      }
    }

    it("can parse offsets without a colon") {
      JsonDecode.fromJValue[OffsetDateTime](JString("1234-10-13T12:34:56-0730")) must equal (Right(OffsetDateTime.parse("1234-10-13T12:34:56-07:30")))
    }

    it("can parse offsets with only an hour") {
      JsonDecode.fromJValue[OffsetDateTime](JString("1234-10-13T12:34:56-07")) must equal (Right(OffsetDateTime.parse("1234-10-13T12:34:56-07:00")))
    }
  }

  describe("OffsetTime codec") {
    it("roundtrips") {
      forAll { (t: OffsetTime) =>
        JsonDecode.fromJValue[OffsetTime](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }

    it("can parse offsets without a colon") {
      JsonDecode.fromJValue[OffsetTime](JString("12:34:56-0730")) must equal (Right(OffsetTime.parse("12:34:56-07:30")))
    }

    it("can parse offsets with only an hour") {
      JsonDecode.fromJValue[OffsetTime](JString("12:34:56-07")) must equal (Right(OffsetTime.parse("12:34:56-07:00")))
    }

    it("can parse an offset of Z") {
      JsonDecode.fromJValue[OffsetTime](JString("12:34:56Z")) must equal (Right(OffsetTime.parse("12:34:56+00:00")))
    }
  }

  describe("ZoneOffset codec") {
    it("roundtrips") {
      forAll { (t: ZoneOffset) =>
        JsonDecode.fromJValue[ZoneOffset](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }

    it("can parse offsets without a colon") {
      JsonDecode.fromJValue[ZoneOffset](JString("-0730")) must equal (Right(ZoneOffset.of("-07:30")))
    }

    it("can parse offsets with only an hour") {
      JsonDecode.fromJValue[ZoneOffset](JString("-07")) must equal (Right(ZoneOffset.of("-07:00")))
    }

    it("can parse Z") {
      JsonDecode.fromJValue[ZoneOffset](JString("Z")) must equal (Right(ZoneOffset.UTC))
    }
  }

  describe("LocalDateTime codec") {
    it("roundtrips") {
      forAll { (t: LocalDateTime) =>
        JsonDecode.fromJValue[LocalDateTime](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }

  describe("LocalDate codec") {
    it("roundtrips") {
      forAll { (t: LocalDate) =>
        JsonDecode.fromJValue[LocalDate](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }

  describe("YearMonth codec") {
    it("roundtrips") {
      forAll { (t: YearMonth) =>
        JsonDecode.fromJValue[YearMonth](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }

  describe("MonthDay codec") {
    it("roundtrips") {
      forAll { (t: MonthDay) =>
        JsonDecode.fromJValue[MonthDay](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }

  describe("LocalTime codec") {
    it("roundtrips") {
      forAll { (t: LocalTime) =>
        JsonDecode.fromJValue[LocalTime](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }

  describe("Duration codec") {
    it("roundtrips") {
      forAll { (t: Duration) =>
        JsonDecode.fromJValue[Duration](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }

    it("handles durations that would be mis-parsed because of JDK-8054978") {
      JsonDecode.fromJValue[Duration](JString("PT-0.3S")) must equal (Right(Duration.ZERO.minusMillis(300)))
      JsonDecode.fromJValue[Duration](JString("PT-2405079928888997H-26M-0.289430578S")).right.map(_.toString) must equal (Right("PT-2405079928888997H-26M-0.289430578S"))
      JsonDecode.fromJValue[Duration](JString("PT2405079928888997H26M-0.289430578S")).right.map(_.toString) must equal (Right("PT2405079928888997H25M59.710569422S"))
      JsonDecode.fromJValue[Duration](JString("PT2405079928888997H-26M-0.289430578S")).right.map(_.toString) must equal (Right("PT2405079928888996H33M59.710569422S"))
    }
  }

  describe("Period codec") {
    it("roundtrips") {
      forAll { (t: Period) =>
        JsonDecode.fromJValue[Period](JsonEncode.toJValue(t)) must equal (Right(t))
      }
    }
  }
}
