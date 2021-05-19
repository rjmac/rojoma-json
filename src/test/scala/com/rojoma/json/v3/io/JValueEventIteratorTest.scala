package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import testsupport.ArbitraryJValue.given
import ast.JValue

class JValueEventIteratorTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Converting to a eventstream and back is an identity") {
    forAll() { (x: JValue) =>
      JsonReader.fromEvents(JValueEventIterator(x)) must equal (x)
    }
  }
}
