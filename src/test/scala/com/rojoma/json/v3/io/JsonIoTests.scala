package com.rojoma.json.v3
package io

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers

import org.scalacheck.Prop._

import testsupport.ArbitraryJValue.ArbitraryJValue
import ast.JValue

import Tokens._

class JsonIoTests extends FunSuite with Checkers with MustMatchers {
  test("object -> compact string -> object") {
    check(forAll { x: JValue =>
      JsonReader(CompactJsonWriter.toString(x)).read() == x
      JsonReader.unbufferedString(CompactJsonWriter.toString(x)).read() == x
      JsonReader.semibufferedString(CompactJsonWriter.toString(x)).read() == x
    })
  }

  test("object -> pretty string -> object") {
    check(forAll { x: JValue =>
      JsonReader(PrettyJsonWriter.toString(x)).read() == x
      JsonReader.unbufferedString(PrettyJsonWriter.toString(x)).read() == x
      JsonReader.semibufferedString(PrettyJsonWriter.toString(x)).read() == x
    })
  }

  test("reading leaves the event iterator empty") {
    check(forAll { x: JValue =>
      val reader = new EventJsonReader(new JsonTokenIterator(new java.io.StringReader(x.toString)))
      reader.read()
      !reader.lexer.hasNext
    })
  }

  test("reading leaves the input iterator positioned on the next token") {
    def tokenAfterDatum(s: String) = {
      val it = new JsonTokenIterator(new java.io.StringReader(s))
      new EventJsonReader(it).read()
      it.next()
    }
    tokenAfterDatum("1 2") must equal (tokenNumber(2))
    tokenAfterDatum("[1,2,3] 4") must equal (tokenNumber(4))
    tokenAfterDatum("{a:1,b:2,c:3} 4") must equal (tokenNumber(4))
  }

  test("Can read more than one thing from a single event reader") {
    check(forAll { (x: JValue, y: JValue) =>
      val r = new EventJsonReader(new JsonTokenIterator(new java.io.StringReader(x.toString + " " + y.toString)))
      val newX = r.read()
      val newY = r.read()
      a [JsonEOF] must be thrownBy { r.read() }
      newX == x && newY == y
    })
  }

  test("Can read more than one thing from a single fused event reader") {
    check(forAll { (x: JValue, y: JValue) =>
      val r = new EventJsonReader(new FusedBlockJsonEventIterator(x.toString + " " + y.toString))
      val newX = r.read()
      val newY = r.read()
      a [JsonEOF] must be thrownBy { r.read() }
      newX == x && newY == y
    })
  }

  test("Can read more than one thing from a single fused reader") {
    check(forAll { (x: JValue, y: JValue) =>
      val r = new FusedBlockJsonReader(x.toString + " " + y.toString)
      val newX = r.read()
      val newY = r.read()
      a [JsonEOF] must be thrownBy { r.read() }
      newX == x && newY == y
    })
  }

  test("reading a partial object throws JsonEOF") {
    a [JsonEOF] must be thrownBy { JsonReader("[1,2,3").read() }
    a [JsonEOF] must be thrownBy { JsonReader.unbufferedString("[1,2,3").read() }
    a [JsonEOF] must be thrownBy { JsonReader.semibufferedString("[1,2,3").read() }
  }

  test("reading a partial string throws JsonEOF") {
    a [JsonEOF] must be thrownBy { JsonReader("'").read() }
    a [JsonEOF] must be thrownBy { JsonReader.unbufferedString("'").read() }
    a [JsonEOF] must be thrownBy { JsonReader.semibufferedString("'").read() }
  }
}
