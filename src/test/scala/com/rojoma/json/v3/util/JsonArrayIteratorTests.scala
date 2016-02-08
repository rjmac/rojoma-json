package com.rojoma.json.v3
package util

import codec.JsonEncode
import ast._
import io._

import testsupport.ArbitraryJValue._

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks

import Events._

class JsonArrayIteratorTests extends FunSuite with MustMatchers with PropertyChecks {
  test("Eventifying an array and decoding it with a JsonArrayIterator is an identity operation") {
    forAll() { xs: List[String] =>
      JsonArrayIterator.fromEvents[String](JValueEventIterator(JsonEncode.toJValue(xs))).toList must equal (xs)
    }
  }

  test("Giving JsonArrayIterator an empty input fails with JsonEOF") {
    a [JsonEOF] must be thrownBy { JsonArrayIterator.fromEvents[String](Iterator.empty) }
  }

  test("Giving JsonArrayIterator a non-array input fails with JsonBadParse") {
    forAll() { x: JValue =>
      whenever(!x.isInstanceOf[JArray]) {
        a [JsonBadParse] must be thrownBy { JsonArrayIterator.fromEvents[String](JValueEventIterator(x)) }
      }
    }
  }

  test("Giving JsonArrayIterator an incomplete array eventually throws JsonEOF") {
    forAll() { x: JArray =>
    a [JsonEOF] must be thrownBy { JsonArrayIterator.fromEvents[JValue](JValueEventIterator(x).toSeq.dropRight(1).iterator).toList } 
    }
  }

  test("Giving JsonArrayIterator input it cannot decode eventually throws JsonArrayIterator.ElementDecodeException") {
    val it = JsonArrayIterator.fromEvents[String](Iterator[JsonEvent](startOfArrayEvent(), stringEvent("hello"), stringEvent("world"), identifierEvent("true"), stringEvent("gnu"), endOfArrayEvent()))
    it.next() must equal ("hello")
    it.next() must equal ("world")
    it.hasNext must be (true)
    a [JsonArrayIterator.ElementDecodeException] must be thrownBy { it.next() }
  }

  test("After throwing an ElementDecodeException, the iterator is positioned after the bad element") {
    val it = JsonArrayIterator.fromEvents[String](Iterator[JsonEvent](startOfArrayEvent(), stringEvent("hello"), stringEvent("world"), identifierEvent("true"), stringEvent("gnu"), endOfArrayEvent()))
    it.next() must equal ("hello")
    it.next() must equal ("world")
    it.hasNext must be (true)
    a [JsonArrayIterator.ElementDecodeException] must be thrownBy { it.next() }
    it.next() must equal ("gnu")
    it.hasNext must be (false)
  }

  test("setting alreadyInArray works") {
    forAll() { xs: JArray =>
      JsonArrayIterator.fromEvents[JValue](JValueEventIterator(xs).drop(1), alreadyInArray = true).toList must equal (xs.toSeq)
    }
  }
}
