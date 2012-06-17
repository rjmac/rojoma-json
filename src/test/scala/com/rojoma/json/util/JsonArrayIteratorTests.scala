package com.rojoma.json
package util

import codec.JsonCodec
import ast._
import io._

import testsupport.ArbitraryJValue._

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.MustMatchers

class JsonArrayIteratorTests extends FunSuite with MustMatchers with PropertyChecks {
  implicit def positionate(evs: Iterator[JsonEvent]) = evs.map(new PositionedJsonEvent(_, -1, -1))

  test("Eventifying an array and decoding it with a JsonArrayIterator is an identity operation") {
    forAll() { xs: List[String] =>
      JsonArrayIterator[String](JValueEventIterator(JsonCodec.toJValue(xs))).toList must equal (xs)
    }
  }

  test("Giving JsonArrayIterator an empty input fails with JsonEOF") {
    evaluating { JsonArrayIterator[String](Iterator.empty) } must produce[JsonEOF]
  }

  test("Giving JsonArrayIterator a non-array input fails with JsonBadParse") {
    forAll() { x: JValue =>
      whenever(!x.isInstanceOf[JArray]) {
        evaluating { JsonArrayIterator[String](JValueEventIterator(x)) } must produce [JsonBadParse]
      }
    }
  }

  test("Giving JsonArrayIterator an incomplete array eventually throws JsonEOF") {
    forAll() { x: JArray =>
      evaluating { JsonArrayIterator[JValue](JValueEventIterator(x).toSeq.dropRight(1).iterator).toList } must produce [JsonEOF]
    }
  }

  test("Giving JsonArrayIterator input it cannot decode eventually throws JsonArrayIterator.ElementDecodeException") {
    val it = JsonArrayIterator[String](Iterator[JsonEvent](StartOfArrayEvent, StringEvent("hello"), StringEvent("world"), IdentifierEvent("true"), StringEvent("gnu"), EndOfArrayEvent))
    it.next() must equal ("hello")
    it.next() must equal ("world")
    it.hasNext must be (true)
    evaluating { it.next() } must produce[JsonArrayIterator.ElementDecodeException]
  }

  test("After throwing an ElementDecodeException, the iterator is positioned after the bad element") {
    val it = JsonArrayIterator[String](Iterator[JsonEvent](StartOfArrayEvent, StringEvent("hello"), StringEvent("world"), IdentifierEvent("true"), StringEvent("gnu"), EndOfArrayEvent))
    it.next() must equal ("hello")
    it.next() must equal ("world")
    it.hasNext must be (true)
    evaluating { it.next() } must produce[JsonArrayIterator.ElementDecodeException]
    it.next() must equal ("gnu")
    it.hasNext must be (false)
  }

  test("setting alreadyInArray works") {
    forAll() { xs: JArray =>
      JsonArrayIterator[JValue](JValueEventIterator(xs).drop(1), alreadyInArray = true).toList must equal (xs.toSeq)
    }
  }
}
