package json
package io

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._

import ast.ArbitraryJson.ArbitraryJson
import ast.JValue

class JsonIoTests extends FunSuite with Checkers {
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

