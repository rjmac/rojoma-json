package com.rojoma.json.v3.util.codecs.time

import java.time._

import org.scalatest.{FunSpec, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary,Gen}

import com.rojoma.json.v3.codec.{JsonDecode, JsonEncode}

class RFC1123Test extends FunSpec with MustMatchers with ScalaCheckPropertyChecks {
  import RFC1123._

  case class SmallInstant(underlying: Instant)
  implicit val smallInstant = Arbitrary {
    for {
      seconds <- Gen.choose(0L, Int.MaxValue*10L)
    } yield SmallInstant(Instant.ofEpochSecond(seconds, 0))
  }

  case class SmallOffsetDateTime(underlying: OffsetDateTime)
  implicit val smallOffsetDateTime = Arbitrary {
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
