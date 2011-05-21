package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import org.scalacheck.Prop._

import ast.ArbitraryJValue.ArbitraryJValue
import ast.JValue

class JsonIoTests extends FunSuite with Checkers {

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
}

