package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import testsupport.ArbitraryJValue.given
import ast._
import codec._

class JsonEventTests extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("JSON events roundtrip without loss") {
    forAll { (v: JValue) =>
      val tokens = new JsonEventIterator(v.toString).toList
      val decoded = JsonDecode.fromJValue[List[JsonEvent]](JsonEncode.toJValue(tokens)).getOrElse(fail("ack"))
      decoded must equal (tokens)
      decoded.map(_.position) must equal (tokens.map(_.position))
    }
  }

  test("JSON events roundtrip sans position without loss except for position") {
    forAll { (v: JValue) =>
      implicit val evCodec = JsonEvent.sansPositionCodec
      val tokens = new JsonEventIterator(v.toString).toList
      val decoded = JsonDecode.fromJValue[List[JsonEvent]](JsonEncode.toJValue(tokens)).getOrElse(fail("ack"))
      decoded must equal (tokens)
      decoded.map(_.position).foreach(_ must equal (Position.Invalid))
    }
  }
}
