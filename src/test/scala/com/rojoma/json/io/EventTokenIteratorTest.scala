package com.rojoma.json
package io

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import testsupport.ArbitraryJValue.ArbitraryJValue
import ast.JValue

class EventTokenIteratorTest extends FunSuite with MustMatchers with PropertyChecks {
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
        EventTokenIterator(Iterator(StartOfArrayEvent()) ++ xs.iterator.flatMap(JValueEventIterator)).toList
      }
    }
  }

  test("End-of-input inside an object open produces a MalformedEventStreamException") {
    forAll(shortList[(String, JValue)]) { xs =>
      a [MalformedEventStreamException] must be thrownBy {
        EventTokenIterator(Iterator(StartOfObjectEvent()) ++ xs.iterator.flatMap { case (k,v) => Iterator(FieldEvent(k)) ++ JValueEventIterator(v) }).toList
      }
    }
  }

  test("A top-level FieldEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(FieldEvent("whatever"))).next()
    }
  }

  test("A top-level EndOfArrayEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(EndOfArrayEvent())).next()
    }
  }

  test("A top-level EndOfObjectEvent must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(EndOfObjectEvent())).next()
    }
  }

  test("Two FieldEvents in a row must produce a MalformedEventStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(StartOfObjectEvent(), FieldEvent("a"), FieldEvent("b"), EndOfObjectEvent())).toList
    }
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(StartOfObjectEvent(), FieldEvent("a"), FieldEvent("b"), StringEvent("c"), EndOfObjectEvent())).toList
    }
  }

  test("A FieldEvent in an array must produce a MalformedStreamException") {
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(StartOfArrayEvent(), StringEvent("x"), FieldEvent("a"), EndOfArrayEvent())).toList
    }
    a [MalformedEventStreamException] must be thrownBy {
      EventTokenIterator(Iterator(StartOfArrayEvent(), StringEvent("x"), FieldEvent("a"), StringEvent("y"), EndOfArrayEvent())).toList
    }
  }

  test("Roundtripping a valid event-stream to tokens must produce the same event-stream") {
    forAll() { jvalue: JValue =>
      val eventStream = JValueEventIterator(jvalue).toList
      new JsonEventIterator(EventTokenIterator(eventStream.iterator)).toList must equal(eventStream)
    }
  }
}
