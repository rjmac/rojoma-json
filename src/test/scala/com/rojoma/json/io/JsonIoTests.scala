package com.rojoma.json
package io

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.Checkers

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import org.scalacheck.Prop._

import testsupport.ArbitraryJValue.ArbitraryJValue
import testsupport.ArbitraryValidString.ArbitraryValidString
import ast.{JValue, JString}

class JsonIoTests extends FunSuite with Checkers with MustMatchers {
  test("object -> compact string -> object") {
    check(forAll { x: JValue =>
      JsonReader.fromString(CompactJsonWriter.toString(x)) == x
    })
  }

  test("object -> pretty string -> object") {
    check(forAll { x: JValue =>
      JsonReader.fromString(PrettyJsonWriter.toString(x)) == x
    })
  }

  test("reading leaves the event iterator empty") {
    check(forAll { x: JValue =>
      val reader = new JsonReader(new JsonTokenIterator(new java.io.StringReader(x.toString)))
      reader.read()
      !reader.lexer.hasNext
    })
  }

  test("reading leaves the input iterator positioned on the next token") {
    def tokenAfterDatum(s: String) = {
      val it = new JsonTokenIterator(new java.io.StringReader(s))
      new JsonReader(it).read()
      it.next()
    }
    tokenAfterDatum("1 2") must equal (TokenNumber(BigDecimal(2)))
    tokenAfterDatum("[1,2,3] 4") must equal (TokenNumber(BigDecimal(4)))
    tokenAfterDatum("{a:1,b:2,c:3} 4") must equal (TokenNumber(BigDecimal(4)))
  }

  test("Can read more than one thing from a single reader") {
    check(forAll { (x: JValue, y: JValue) =>
      val r = new JsonReader(new JsonTokenIterator(new java.io.StringReader(x.toString + " " + y.toString)))
      val newX = r.read()
      val newY = r.read()
      a [JsonEOF] must be thrownBy { r.read() }
      newX == x && newY == y
    })
  }

  test("reading a partial object throws JsonEOF") {
    a [JsonEOF] must be thrownBy { JsonReader.fromString("[1,2,3") }
  }

  test("reading a partial string throws JsonEOF") {
    a [JsonEOF] must be thrownBy { JsonReader.fromString("'") }
  }
}
