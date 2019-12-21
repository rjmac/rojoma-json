package com.rojoma.json.v3.util.codecs

import java.time._

import org.scalacheck.{Arbitrary, Gen}

package object time {
  val nanosPerSecond = 1000000000L

  implicit val ArbitraryInstant = Arbitrary {
    assert(Instant.MIN.getNano == 0)
    assert(Instant.MAX.getNano == nanosPerSecond - 1)
    for {
      seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
      nanos <- Gen.choose(0, nanosPerSecond - 1)
    } yield Instant.ofEpochSecond(seconds, nanos)
  }

  implicit val ArbitraryOffset = Arbitrary {
    for {
      seconds <- Gen.choose(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds)
    } yield ZoneOffset.ofTotalSeconds(seconds)
  }

  implicit val ArbitraryOffsetDateTime = Arbitrary {
    for {
      instant <- Arbitrary.arbitrary[Instant]
      offset <- Arbitrary.arbitrary[ZoneOffset]
    } yield instant.atOffset(offset)
  }

  implicit val ArbitraryOffsetTime = Arbitrary {
    for {
      offsetDateTime <- Arbitrary.arbitrary[OffsetDateTime]
    } yield offsetDateTime.toOffsetTime
  }

  implicit val ArbitraryLocalDateTime = Arbitrary {
    for {
      instant <- Arbitrary.arbitrary[Instant]
    } yield instant.atOffset(ZoneOffset.UTC).toLocalDateTime
  }

  implicit val ArbitraryLocalDate = Arbitrary {
    for {
      localDateTime <- Arbitrary.arbitrary[LocalDateTime]
    } yield localDateTime.toLocalDate
  }

  implicit val ArbitraryYearMonth = Arbitrary {
    for {
      localDate <- Arbitrary.arbitrary[LocalDate]
    } yield YearMonth.of(localDate.getYear, localDate.getMonth)
  }

  implicit val ArbitraryMonthDay = Arbitrary {
    for {
      localDate <- Arbitrary.arbitrary[LocalDate]
    } yield MonthDay.of(localDate.getMonth, localDate.getDayOfMonth)
  }

  implicit val ArbitraryLocalTime = Arbitrary {
    for {
      localDateTime <- Arbitrary.arbitrary[LocalDateTime]
    } yield localDateTime.toLocalTime
  }

  implicit val ArbitraryDuration = Arbitrary {
    for {
      seconds <- Arbitrary.arbitrary[Long]
      nanos <- Gen.choose(0, nanosPerSecond - 1)
    } yield Duration.ofSeconds(seconds, nanos)
  }

  implicit val ArbitraryPeriod = Arbitrary {
    for {
      years <- Arbitrary.arbitrary[Int]
      months <- Arbitrary.arbitrary[Int]
      days <- Arbitrary.arbitrary[Int]
    } yield Period.of(years, months, days)
  }
}
