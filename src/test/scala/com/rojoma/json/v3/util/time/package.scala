package com.rojoma.json.v3.util.time

import java.time._

import org.scalacheck.{Arbitrary, Gen}

val nanosPerSecond = locally {
  val nps = 1000000000L
  assert(Instant.MIN.getNano == 0)
  assert(Instant.MAX.getNano == nps - 1)
  nps
}

private val arbitraryNanos =
  Gen.oneOf(
    Gen.const(0L),
    Gen.choose(0, 999).map(_ * (nanosPerSecond / 1000)),
    Gen.choose(0L, nanosPerSecond - 1)
  )

private val fullyArbitraryInstant =
  for {
    seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
    nanos <- arbitraryNanos
  } yield Instant.ofEpochSecond(seconds, nanos)

private val arbitraryReasonableInstant =
  for {
    seconds <- Gen.choose(0L, 1L << 33)
    nanos <- arbitraryNanos
  } yield Instant.ofEpochSecond(seconds, nanos)

given ArbitraryInstant: Arbitrary[Instant] = Arbitrary {
  Gen.oneOf(
    fullyArbitraryInstant,
    arbitraryReasonableInstant
  )
}

given ArbitraryOffset: Arbitrary[ZoneOffset] = Arbitrary {
  for {
    seconds <- Gen.choose(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds)
  } yield ZoneOffset.ofTotalSeconds(seconds)
}

given ArbitraryOffsetDateTime: Arbitrary[OffsetDateTime] = Arbitrary {
  for {
    instant <- Arbitrary.arbitrary[Instant]
    offset <- Arbitrary.arbitrary[ZoneOffset]
  } yield instant.atOffset(offset)
}

given ArbitraryOffsetTime: Arbitrary[OffsetTime] = Arbitrary {
  for {
    offsetDateTime <- Arbitrary.arbitrary[OffsetDateTime]
  } yield offsetDateTime.toOffsetTime
}

given ArbitraryLocalDateTime: Arbitrary[LocalDateTime] = Arbitrary {
  for {
    instant <- Arbitrary.arbitrary[Instant]
  } yield instant.atOffset(ZoneOffset.UTC).toLocalDateTime
}

given ArbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary {
  for {
    localDateTime <- Arbitrary.arbitrary[LocalDateTime]
  } yield localDateTime.toLocalDate
}

given ArbitraryYearMonth: Arbitrary[YearMonth] = Arbitrary {
  for {
    localDate <- Arbitrary.arbitrary[LocalDate]
  } yield YearMonth.of(localDate.getYear, localDate.getMonth)
}

given ArbitraryMonthDay: Arbitrary[MonthDay] = Arbitrary {
  for {
    localDate <- Arbitrary.arbitrary[LocalDate]
  } yield MonthDay.of(localDate.getMonth, localDate.getDayOfMonth)
}

given ArbitraryLocalTime: Arbitrary[LocalTime] = Arbitrary {
  for {
    localDateTime <- Arbitrary.arbitrary[LocalDateTime]
  } yield localDateTime.toLocalTime
}

given ArbitraryDuration: Arbitrary[Duration] = Arbitrary {
  for {
    seconds <- Arbitrary.arbitrary[Long]
    nanos <- Gen.choose(0L, nanosPerSecond - 1)
  } yield Duration.ofSeconds(seconds, nanos)
}

given ArbitraryPeriod: Arbitrary[Period] = Arbitrary {
  for {
    years <- Arbitrary.arbitrary[Int]
    months <- Arbitrary.arbitrary[Int]
    days <- Arbitrary.arbitrary[Int]
  } yield Period.of(years, months, days)
}
