package com.rojoma.json.v3
package ast

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

class JValueTests extends FunSuite with Checkers {
  test("byte roundtrips") {
    check(forAll { x: Byte =>
      JNumber(x).toByte == x
    })
  }

  test("short roundtrips") {
    check(forAll { x: Short =>
      JNumber(x).toShort == x
    })
  }

  test("int roundtrips") {
    check(forAll { x: Int =>
      JNumber(x).toInt == x
    })
  }

  test("long roundtrips") {
    check(forAll { x: Long =>
      JNumber(x).toLong == x
    })
  }

  test("bigint roundtrips") {
    check(forAll { x: BigInt =>
      JNumber(x).toBigInt == x
    })
  }

  test("float roundtrips") {
    check(forAll { x: Float =>
      JNumber(x).toFloat == x
    })
  }

  test("double roundtrips") {
    check(forAll { x: Double =>
      JNumber(x).toDouble == x
    })
  }
}
