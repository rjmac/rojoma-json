package com.rojoma.json.v3
package io

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import testsupport.ArbitraryJValue.ArbitraryJValue
import ast.JValue

class JValueEventIteratorTest extends FunSuite with MustMatchers with ScalaCheckPropertyChecks {
  test("Converting to a eventstream and back is an identity") {
    forAll() { x: JValue =>
      JsonReader.fromEvents(JValueEventIterator(x)) must equal (x)
    }
  }
}
