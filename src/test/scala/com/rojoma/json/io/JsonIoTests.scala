package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.matchers.MustMatchers

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import org.scalacheck.Prop._

import ast.ArbitraryJValue.ArbitraryJValue
import ast.{JValue, JString}

class JsonIoTests extends FunSuite with Checkers with MustMatchers {

  // Strings with surrogate characters that make valid surrogate pairs
  implicit val ArbitraryValidString = Arbitrary[String] {
    val lowSurrogate = Gen.choose(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE).map(_.toChar)
    
    val notLowSurrogate = Gen.frequency(
      (Character.MIN_LOW_SURROGATE - Char.MinValue, Gen.choose(Char.MinValue, Character.MIN_LOW_SURROGATE - 1)),
      (Char.MaxValue - Character.MAX_LOW_SURROGATE, Gen.choose(Character.MAX_LOW_SURROGATE + 1, Char.MaxValue))
    ).map(_.toChar)
    
    val validCodePoint = notLowSurrogate flatMap { a =>
      if(a.isHighSurrogate) lowSurrogate map { b => new String(Array(a, b)) }
      else a.toString
    }

    Gen.containerOf[List, String](validCodePoint) map (_.mkString)
  }

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

  test("reading replaces broken surrogate pairs") {
    JsonReader.fromString("'\ud800'") must equal (JString("\ufffd"))
    JsonReader.fromString("'\ud800x'") must equal (JString("\ufffdx"))
    JsonReader.fromString("'\udc00'") must equal (JString("\ufffd"))
    JsonReader.fromString("'\udc00x'") must equal (JString("\ufffdx"))
    JsonReader.fromString("'\udc00\ud800\udc00'") must equal (JString("\ufffd\ud800\udc00"))

    JsonReader.fromString("'\\ud800'") must equal (JString("\ufffd"))
    JsonReader.fromString("'\\ud800x'") must equal (JString("\ufffdx"))
    JsonReader.fromString("'\\udc00'") must equal (JString("\ufffd"))
    JsonReader.fromString("'\\udc00x'") must equal (JString("\ufffdx"))
    JsonReader.fromString("'\\udc00\\ud800\\udc00'") must equal (JString("\ufffd\ud800\udc00"))
  }

  test("reading handles mixed escaped/unescaped surrogate pairs") {
    JsonReader.fromString("'\\ud800\udc00'") must equal (JString("\ud800\udc00"))
    JsonReader.fromString("'\ud800\\udc00'") must equal (JString("\ud800\udc00"))
  }
}

