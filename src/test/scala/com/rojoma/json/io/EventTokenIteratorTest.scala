package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.MustMatchers

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
    evaluating(EventTokenIterator(Iterator.empty).next()) must produce[NoSuchElementException]
  }

  test("End-of-input inside an array open produces a MalformedEventStreamException") {
    forAll(shortList[JValue]) { xs =>
      evaluating(EventTokenIterator(Iterator(StartOfArrayEvent) ++ xs.iterator.flatMap(JValueEventIterator)).toList) must produce[MalformedEventStreamException]
    }
  }

  test("End-of-input inside an object open produces a MalformedEventStreamException") {
    forAll(shortList[(String, JValue)]) { xs =>
      evaluating(EventTokenIterator(Iterator(StartOfObjectEvent) ++ xs.iterator.flatMap { case (k,v) => Iterator(FieldEvent(k)) ++ JValueEventIterator(v) }).toList) must produce[MalformedEventStreamException]
    }
  }

  test("A top-level FieldEvent must produce a MalformedEventStreamException") {
    evaluating(EventTokenIterator(Iterator(FieldEvent("whatever"))).next()) must produce[MalformedEventStreamException]
  }

  test("A top-level EndOfArrayEvent must produce a MalformedEventStreamException") {
    evaluating(EventTokenIterator(Iterator(EndOfArrayEvent)).next()) must produce[MalformedEventStreamException]
  }

  test("A top-level EndOfObjectEvent must produce a MalformedEventStreamException") {
    evaluating(EventTokenIterator(Iterator(EndOfObjectEvent)).next()) must produce[MalformedEventStreamException]
  }

  test("Two FieldEvents in a row must produce a MalformedEventStreamException") {
    evaluating(EventTokenIterator(Iterator(StartOfObjectEvent, FieldEvent("a"), FieldEvent("b"), EndOfObjectEvent)).toList) must produce[MalformedEventStreamException]
    evaluating(EventTokenIterator(Iterator(StartOfObjectEvent, FieldEvent("a"), FieldEvent("b"), StringEvent("c"), EndOfObjectEvent)).toList) must produce[MalformedEventStreamException]
  }

  test("A FieldEvent in an array must produce a MalformedStreamException") {
    evaluating(EventTokenIterator(Iterator(StartOfArrayEvent, StringEvent("x"), FieldEvent("a"), EndOfArrayEvent)).toList) must produce[MalformedEventStreamException]
    evaluating(EventTokenIterator(Iterator(StartOfArrayEvent, StringEvent("x"), FieldEvent("a"), StringEvent("y"), EndOfArrayEvent)).toList) must produce[MalformedEventStreamException]
  }

  test("Roundtripping a valid event-stream to tokens must produce the same event-stream") {
    forAll() { jvalue: JValue =>
      val eventStream = JValueEventIterator(jvalue).toList
      new JsonEventIterator(EventTokenIterator(eventStream.iterator).map(new PositionedJsonToken(_, -1, -1))).map(_.event).toList must equal(eventStream)
    }
  }
}
