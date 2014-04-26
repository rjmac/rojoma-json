package com.rojoma.json
package io

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks

import testsupport.ArbitraryJValue.ArbitraryJValue
import ast.JValue

class JValueEventIteratorTest extends FunSuite with MustMatchers with PropertyChecks {
  test("Converting to a eventstream and back is an identity") {
    forAll() { x: JValue =>
      JsonReader.fromEvents(JValueEventIterator(x)) must equal (x)
    }
  }
}
