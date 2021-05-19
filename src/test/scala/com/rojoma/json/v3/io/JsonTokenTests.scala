package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import testsupport.ArbitraryJValue.given
import ast._
import codec._

class JsonTokenTests extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("JSON tokens roundtrip without loss") {
    forAll { (v: JValue) =>
      val tokens = new BlockJsonTokenIterator(v.toString).toList
      val decoded = JsonDecode.fromJValue[List[JsonToken]](JsonEncode.toJValue(tokens)).getOrElse(fail("ack"))
      decoded must equal (tokens)
      decoded.map(_.position) must equal (tokens.map(_.position))
    }
  }

  test("JSON tokens roundtrip sans position without loss except for position") {
    forAll { (v: JValue) =>
      implicit val evCodec = JsonToken.sansPositionCodec
      val tokens = new BlockJsonTokenIterator(v.toString).toList
      val decoded = JsonDecode.fromJValue[List[JsonToken]](JsonEncode.toJValue(tokens)).getOrElse(fail("ack"))
      decoded must equal (tokens)
      decoded.map(_.position).foreach(_ must equal (Position.Invalid))
    }
  }
}
