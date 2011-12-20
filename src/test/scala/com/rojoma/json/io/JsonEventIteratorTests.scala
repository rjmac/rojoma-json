package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class JsonEventIteratorTests extends FunSuite with MustMatchers {
  def r(s: String) = new java.io.StringReader(s)
  def i(s: String) = new JsonEventIterator(new TokenIterator(r(s)))
  def e(s: String) = i(s).next().event

  test("reading single tokens that are legitimate start-of-datum tokens") {
    e("\"hello\"") must equal (StringEvent("hello"))
    e("true") must equal (IdentifierEvent("true"))
    e("1.432") must equal (NumberEvent(BigDecimal("1.432")))
    e("[") must equal (StartOfArrayEvent)
    e("{") must equal (StartOfObjectEvent)
  }

  test("reading non start-of-datum tokens fails") {
    evaluating { e("]") } must produce [JsonUnexpectedToken]
    evaluating { e("}") } must produce [JsonUnexpectedToken]
    evaluating { e(":") } must produce [JsonUnexpectedToken]
    evaluating { e(",") } must produce [JsonUnexpectedToken]
  }

  test("skipRestOfCompound'ing from next() before calling next() does nothing") {
    i("[1,2,3]").skipRestOfCompound().next().event must equal (StartOfArrayEvent)
  }

  test("skipRestOfCompound'ing from head before calling next() skips first element") {
    val it = i("[1,2,3]")
    it.skipRestOfCompound(fromHead = true).toSeq must be ('empty)
  }

  test("skipRestOfCompound'ing after calling next() to enter skips rest of the datum") {
    var it = i("[1,2,3]")
    it.next()
    it.skipRestOfCompound().toSeq must be ('empty)

    it = i("{hello:'world',smiling:'gnus'}")
    it.next()
    it.skipRestOfCompound().toSeq must be ('empty)
  }

  test("skipRestOfCompound'ing from head after calling next() to enter skips rest of the datum, if the first item is an atom") {
    var it = i("[1,2,3]")
    it.next()
    it.skipRestOfCompound(fromHead = true).toSeq must be ('empty)

    it = i("{hello:'world',smiling:'gnus'}")
    it.next()
    it.next().event must equal (FieldEvent("hello")) // skip the field event
    it.skipRestOfCompound(fromHead = true).toSeq must be ('empty)
  }

  test("skipRestOfCompound'ing from head after calling next() to enter skips the current datum, if the first item is compound") {
    var it = i("[['a','b','c'],2,3]")
    it.next()
    it.skipRestOfCompound(fromHead = true).next().event must equal (NumberEvent(BigDecimal(2)))

    it = i("{hello:['a','b','c'],smiling:'gnus'}")
    it.next()
    it.next().event must equal (FieldEvent("hello")) // skip the field event
    it.skipRestOfCompound(fromHead = true).next().event must equal (FieldEvent("smiling"))
  }

  test("skipRestOfCompound'ing at the end raises NoSuchElementException") {
    var it = i("5")
    it.next()
    evaluating { it.skipRestOfCompound() } must produce [NoSuchElementException]
  }

  test("skipNextDatum at the top level reads a whole object") {
    i("5").skipNextDatum().toSeq must be ('empty)
    i("[1,2,3]").skipNextDatum().toSeq must be ('empty)
  }

  test("skipNextDatum within an array skips one item") {
    var it = i("[1,2,3]")
    it.next()
    it.skipNextDatum().next().event must equal (NumberEvent(BigDecimal(2)))
  }

  test("skipNextDatum at the end of an array does not move") {
    var it = i("[1]")
    it.next()
    it.next()
    it.skipNextDatum().next().event must equal (EndOfArrayEvent)
  }

  test("skipNextDatum within an object skips both field and datum") {
    var it = i("{'hello':'world','smiling','gnus'}")
    it.next()
    it.skipNextDatum().next().event must equal (FieldEvent("smiling"))

    it = i("{'hello':'world','smiling','gnus'}")
    it.next()
    it.next() // position before "world"
    it.skipNextDatum().next().event must equal (FieldEvent("smiling"))
  }


  test("skipNextDatum at the end of object does not move") {
    var it = i("{'hello':'world'}")
    it.next()
    it.next()
    it.next()
    it.skipNextDatum().next().event must equal (EndOfObjectEvent)
  }
}
