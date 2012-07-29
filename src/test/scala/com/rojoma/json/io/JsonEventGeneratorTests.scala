package com.rojoma.json
package io

import ast.JValue
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.PropertyChecks

class JsonEventGeneratorTests extends FunSuite with MustMatchers with PropertyChecks {
  def r(s: String) = new java.io.StringReader(s)
  def doParse(s: String) = {
    val it = new JsonTokenIterator(r(s))
    val (acc, finalParser) = it.foldLeft((List.empty[JsonEvent], JsonEventGenerator.newGenerator)) { (accParser, token) =>
      val (acc, parser) = accParser
      parser.parse(token) match {
        case JsonEventGenerator.Event(ev, newGenerator) =>
          ev.position.isValid must be (true)
          (ev :: acc, newGenerator)
        case JsonEventGenerator.More(newGenerator) =>
          (acc, newGenerator)
      }
    }
    (acc.reverse, finalParser)
  }
  def parseAll(s: String, requireComplete: Boolean = false) = {
    val (acc, finalParser) = doParse(s)
    if(requireComplete) finalParser.atTopLevel must be (true)
    acc
  }
  def e(s: String) = parseAll(s).head

  test("reading single tokens that are legitimate start-of-datum tokens") {
    e("\"hello\"") must equal (StringEvent("hello"))
    e("true") must equal (IdentifierEvent("true"))
    e("1.432") must equal (NumberEvent(BigDecimal("1.432")))
    e("[") must equal (StartOfArrayEvent())
    e("{") must equal (StartOfObjectEvent())
  }

  test("reading non start-of-datum tokens fails") {
    evaluating { e("]") } must produce [JsonUnexpectedToken]
    evaluating { e("}") } must produce [JsonUnexpectedToken]
    evaluating { e(":") } must produce [JsonUnexpectedToken]
    evaluating { e(",") } must produce [JsonUnexpectedToken]
  }

  test("Parsing list") {
    parseAll("[1,2,'gnu']", requireComplete = true) must equal (List(StartOfArrayEvent(), NumberEvent(1), NumberEvent(2), StringEvent("gnu"), EndOfArrayEvent()))
  }

  test("Parsing object") {
    parseAll("{gnu:1,'gnat':2,'gnarf':'3'}", requireComplete = true) must equal (List(
      StartOfObjectEvent(), FieldEvent("gnu"), NumberEvent(1), FieldEvent("gnat"), NumberEvent(2), FieldEvent("gnarf"), StringEvent("3"), EndOfObjectEvent()))
  }

  test("Parsing random JSON works") {
    forAll { j: JValue =>
      JsonReader.fromEvents(parseAll(j.toString, requireComplete = true).iterator) must equal (j)
    }
  }

  test("A field must be a string or identifier") {
    parseAll("{'a'")
    parseAll("{a")
    evaluating { parseAll("{5") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{[") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{{") } must produce [JsonUnexpectedToken]
  }

  test("A field must be followed by a colon") {
    parseAll("{'a':")
    parseAll("{a:")
    evaluating { parseAll("{'a' 'a'") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{'a' a") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{'a' ,") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{'a' 5") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{'a' {") } must produce [JsonUnexpectedToken]
    evaluating { parseAll("{'a' [") } must produce [JsonUnexpectedToken]
  }

  test("Array items must be separated by commas") {
    parseAll("[1,2")
    evaluating { parseAll("[1 2") } must produce [JsonUnexpectedToken]
  }

  test("Object fields must be separated by commas") {
    parseAll("{'a':1,'b':2")
    evaluating { parseAll("{'a':1 'b':2") } must produce [JsonUnexpectedToken]
  }

  test("A brand-new parser must be at toplevel") {
    JsonEventGenerator.newGenerator must be ('atTopLevel)
  }

  test("Reading an atom leaves the parser at toplevel") {
    doParse("1")._2 must be ('atTopLevel)
    doParse("true")._2 must be ('atTopLevel)
    doParse("'hello'")._2 must be ('atTopLevel)
  }

  test("Reading a complete compound leaves the parser at toplevel") {
    doParse("[]")._2 must be ('atTopLevel)
    doParse("{}")._2 must be ('atTopLevel)
    doParse("[1,2,3]")._2 must be ('atTopLevel)
    doParse("{a:1,b:2}")._2 must be ('atTopLevel)
  }

  test("Reading a partial compound leaves the parser not at toplevel") {
    doParse("[")._2 must not be ('atTopLevel)
    doParse("{")._2 must not be ('atTopLevel)
    doParse("[[1,2,3]")._2 must not be ('atTopLevel)
    doParse("{a:1,b:2")._2 must not be ('atTopLevel)
  }
}
