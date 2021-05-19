package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import testsupport.ArbitraryJValue.ArbitraryJValue
import ast.JValue
import Events._

class EventTokenIteratorTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  def shortList[T : Arbitrary] = for {
    size <- Gen.choose(0, 5)
    list <- Gen.listOfN(size, Arbitrary.arbitrary[T])
  } yield list

  test("Empty input produces empty output") {
    EventTokenIterator(Iterator.empty).hasNext must be (false)
  }

  test("next() on an empty EventTokenIterator produces NoSuchElementException") {
    a [NoSuchElementException] must be thrownBy {
      EventTokenIterator(Iterator.empty).next()
    }
  }

  test("End-of-input inside an array open produces a MalformedEventStreamException") {
    forAll(shortList[JValue]) { xs =>
      a [MalformedEventStreamException] must be thrownBy {
        EventTokenIterator(Iterator(startOfArrayEvent()) ++ xs.iterator.flatMap(JValueEventIterator)).toList
      }
    }
  }

  test("End-of-input inside an object open produces a MalformedEventStreamException") {
    forAll(shortList[(String, JValue)]) { xs =>
      a [MalformedEventStreamException] must be thrownBy {
        EventTokenIterator(Iterator(startOfObjectEvent()) ++ xs.iterator.flatMap { case (k,v) => Iterator(fieldEvent(k)) ++ JValueEventIterator(v) }).toList
      }
    }
  }

  test("A top-level FieldEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(fieldEvent("whatever"))).next()
    }
  }

  test("A top-level EndOfArrayEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(endOfArrayEvent())).next()
    }
  }

  test("A top-level EndOfObjectEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(endOfObjectEvent())).next()
    }
  }

  test("Two FieldEvents in a row must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(startOfObjectEvent(), fieldEvent("a"), fieldEvent("b"), endOfObjectEvent())).toList
    }
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(startOfObjectEvent(), fieldEvent("a"), fieldEvent("b"), stringEvent("c"), endOfObjectEvent())).toList
    }
  }

  test("A FieldEvent in an array must produce a MalformedStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(startOfArrayEvent(), stringEvent("x"), fieldEvent("a"), endOfArrayEvent())).toList
    }
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(startOfArrayEvent(), stringEvent("x"), fieldEvent("a"), stringEvent("y"), endOfArrayEvent())).toList
    }
  }

  test("Roundtripping a valid event-stream to tokens must produce the same event-stream") {
    forAll() { (jvalue: JValue) =>
      val eventStream = JValueEventIterator(jvalue).toList
      new JsonEventIterator(EventTokenIterator(eventStream.iterator)).toList must equal(eventStream)
    }
  }
}
