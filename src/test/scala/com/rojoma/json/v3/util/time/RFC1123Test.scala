package com.rojoma.json.v3.util.time

import java.time._

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary,Gen}

import com.rojoma.json.v3.codec.{JsonDecode, JsonEncode}

class RFC1123Test extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  import RFC1123.codec.given

  case class SmallInstant(underlying: Instant)
  given smallInstant: Arbitrary[SmallInstant] = Arbitrary {
    for {
      seconds <- Gen.choose(0L, Int.MaxValue*10L)
    } yield SmallInstant(Instant.ofEpochSecond(seconds, 0))
  }

  case class SmallOffsetDateTime(underlying: OffsetDateTime)
  given smallOffsetDateTime: Arbitrary[SmallOffsetDateTime] = Arbitrary {
    for {
      instant <- Arbitrary.arbitrary(smallInstant).map(_.underlying)
      offset <- Arbitrary.arbitrary[ZoneOffset]
    } yield SmallOffsetDateTime(instant.atOffset(ZoneOffset.ofTotalSeconds(offset.getTotalSeconds / 60 * 60)))
  }

  describe("Instant codec") {
    it("roundtrips") {
      forAll { (t: SmallInstant) =>
        JsonDecode.fromJValue[Instant](JsonEncode.toJValue(t.underlying)) must equal (Right(t.underlying))
      }
    }

    it("reads timestamps with offsets") {
      forAll { (t: SmallOffsetDateTime) =>
        JsonDecode.fromJValue[Instant](JsonEncode.toJValue(t.underlying)) must equal (Right(t.underlying.toInstant))
      }
    }
  }

  describe("OffsetDateTime codec") {
    it("roundtrips") {
      forAll { (t: SmallOffsetDateTime) =>
        JsonDecode.fromJValue[OffsetDateTime](JsonEncode.toJValue(t.underlying)) must equal (Right(t.underlying))
      }
    }
  }
}
