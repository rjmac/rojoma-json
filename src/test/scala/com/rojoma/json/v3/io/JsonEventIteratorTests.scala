package com.rojoma.json.v3
package io

import org.scalatest.{FunSuite, MustMatchers}

import Events._

class JsonEventIteratorTests extends FunSuite with MustMatchers {
  def r(s: String) = new java.io.StringReader(s)
  def i(s: String) = new JsonEventIterator(new JsonTokenIterator(r(s)))
  def e(s: String) = i(s).next()

  test("reading single tokens that are legitimate start-of-datum tokens") {
    e("\"hello\"") must equal (stringEvent("hello"))
    e("true") must equal (identifierEvent("true"))
    e("1.432") must equal (numberEvent("1.432"))
    e("[") must equal (startOfArrayEvent())
    e("{") must equal (startOfObjectEvent())
  }

  test("reading non start-of-datum tokens fails") {
    a [JsonUnexpectedToken] must be thrownBy { e("]") }
    a [JsonUnexpectedToken] must be thrownBy { e("}") }
    a [JsonUnexpectedToken] must be thrownBy { e(":") }
    a [JsonUnexpectedToken] must be thrownBy { e(",") }
  }

  test("Calling next() on an event iterator reads no more than is necessary") {
    val reader = new java.io.BufferedReader(r("1 2"))
    val it = new JsonEventIterator(reader)
    it.next()
    reader.readLine() must equal ("2")
  }

  test("skipRestOfCompound() before calling next() does nothing") {
    i("[1,2,3]").skipRestOfCompound().next() must equal (startOfArrayEvent())
  }

  test("skipRestOfCompound() before next() but after hasNext does nothing") {
    val it = i("[1,2,3]")
    it.hasNext
    it.skipRestOfCompound().next() must equal (startOfArrayEvent())
  }

  test("skipRestOfCompound() after calling next() to enter skips rest of the datum") {
    var it = i("[1,2,3]")
    it.next()
    it.skipRestOfCompound().toSeq must be ('empty)

    it = i("[['a','b','c'],2,3]")
    it.next()
    it.skipRestOfCompound().toSeq must be ('empty)

    it = i("{hello:'world',smiling:'gnus'}")
    it.next()
    it.skipRestOfCompound().toSeq must be ('empty)
  }

  test("skipRestOfCompound() within a nested object skips the rest of the inner datum") {
    val it = i("[['a','b','c'],2,3]")
    it.next() must equal (startOfArrayEvent())
    it.next() must equal (startOfArrayEvent())
    it.skipRestOfCompound().next() must equal (numberEvent(2))
  }

  test("skipNextDatum() in a multi-object stream leaves it positioned at the start of next object") {
    val it = i("[1,2,3] 'gnu'")
    it.skipNextDatum()
    it.next() must equal (stringEvent("gnu"))
  }

  test("skipRestOfCompound() between top-level objects does nothing") {
    val it = i("[1,2,3] 'gnu'")
    it.skipNextDatum()
    it.skipRestOfCompound().next() must equal (stringEvent("gnu"))
  }

  test("skipRestOfCompound() at the end does not raise NoSuchElementException") {
    val it = i("5")
    it.next()
    it.skipRestOfCompound()
    it.hasNext must be (false)
  }

  test("skipRestOfCompound() in an incomplete object raises JsonEOF") {
    val it = i("[1,2,3")
    it.next()
    a [JsonEOF] must be thrownBy { it.skipRestOfCompound() }
  }

  test("skipRestOfCompound() in an incomplete object raises a parse exception") {
    val it = i("[1,2,3")
    it.next()
    a [JsonParseException] must be thrownBy { it.skipRestOfCompound() }
  }

  test("skipNextDatum() at EOF produces NoSuchElementException") {
    val it = i("")
    a [NoSuchElementException] must be thrownBy { it.skipNextDatum() }
  }

  test("skipNextDatum() at the top level reads a whole object") {
    i("5").skipNextDatum().toSeq must be ('empty)
    i("[1,2,3]").skipNextDatum().toSeq must be ('empty)
  }

  test("skipNextDatum() of an incomplete object raises JsonEOF") {
    val it = i("[1,2,3")
    a [JsonEOF] must be thrownBy { it.skipNextDatum() }
  }

  test("skipNextDatum() of an incomplete object raises a parse eception") {
    val it = i("[1,2,3")
    a [JsonParseException] must be thrownBy { it.skipNextDatum() }
  }

  test("skipNextDatum() within an array skips one item") {
    val it = i("[1,2,3]")
    it.next()
    it.skipNextDatum().next() must equal (numberEvent(2))
  }

  test("skipNextDatum() at the end of an array does not move") {
    val it = i("[1]")
    it.next()
    it.next()
    it.skipNextDatum().next() must equal (endOfArrayEvent())
  }

  test("skipNextDatum() within an object skips both field and datum") {
    var it = i("{'hello':'world','smiling','gnus'}")
    it.next()
    it.skipNextDatum()
    it.next() must equal (fieldEvent("smiling"))

    it = i("{'hello':'world','smiling','gnus'}")
    it.next()
    it.next() // position before "world"
    it.skipNextDatum()
    it.next() must equal (fieldEvent("smiling"))
  }


  test("skipNextDatum() at the end of object does not move") {
    val it = i("{'hello':'world'}")
    it.next()
    it.next()
    it.next() must equal (stringEvent("world"))
    it.skipNextDatum().next() must equal (endOfObjectEvent())
  }
}
