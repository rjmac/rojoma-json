package com.rojoma.json.v3
package io

import ast.JValue
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks

import Events._

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

  test("Parsing list") {
    parseAll("[1,2,'gnu']", requireComplete = true) must equal (List(startOfArrayEvent(), numberEvent(1), numberEvent(2), stringEvent("gnu"), endOfArrayEvent()))
  }

  test("Parsing object") {
    parseAll("{gnu:1,'gnat':2,'gnarf':'3'}", requireComplete = true) must equal (List(
      startOfObjectEvent(), fieldEvent("gnu"), numberEvent(1), fieldEvent("gnat"), numberEvent(2), fieldEvent("gnarf"), stringEvent("3"), endOfObjectEvent()))
  }

  test("Parsing random JSON works") {
    forAll { j: JValue =>
      JsonReader.fromEvents(parseAll(j.toString, requireComplete = true).iterator) must equal (j)
    }
  }

  test("A field must be a string or identifier") {
    parseAll("{'a'")
    parseAll("{a")
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{5") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{[") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{{") }
  }

  test("A field must be followed by a colon") {
    parseAll("{'a':")
    parseAll("{a:")
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' 'a'") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' a") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' ,") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' 5") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' {") }
    a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a' [") }
  }

  test("Array items must be separated by commas") {
    parseAll("[1,2")
    a [JsonUnexpectedToken] must be thrownBy { parseAll("[1 2") }
  }

  test("Object fields must be separated by commas") {
    parseAll("{'a':1,'b':2")
   a [JsonUnexpectedToken] must be thrownBy { parseAll("{'a':1 'b':2") }
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
