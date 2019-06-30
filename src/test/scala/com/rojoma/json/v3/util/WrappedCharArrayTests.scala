package com.rojoma.json.v3
package util

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class WrappedCharArrayTests extends FunSuite with MustMatchers with ScalaCheckPropertyChecks {
  test("Converting a WrappedCharArray back into a String succeeds") {
    WrappedCharArray("hello there".toCharArray).toString must equal ("hello there")
  }

  test("Converting a WrappedCharArray to a CharBuffer succeeds") {
    WrappedCharArray("hello there".toCharArray).toCharBuffer.isReadOnly must be (true)
  }

  test("Extracting a slice of a char array works") {
    val wca = WrappedCharArray("hello there".toCharArray, 3, 5)
    wca.length must be (5)
    wca(0) must be ('l')
    wca(4) must be ('h')
    wca.toString must be ("lo th")
  }

  test("Wrapping a substring works") {
    val wca = WrappedCharArray("hello there".substring(3, 8).toCharArray)
    wca.length must be (5)
    wca(0) must be ('l')
    wca(4) must be ('h')
    wca.toString must be ("lo th")
  }

  test("Bounds-checking on a slice works") {
    an [IndexOutOfBoundsException] must be thrownBy { WrappedCharArray("hello there".toCharArray, -1, 5) }
    an [IndexOutOfBoundsException] must be thrownBy { WrappedCharArray("hello there".toCharArray, 3, -1) }
    an [IndexOutOfBoundsException] must be thrownBy { WrappedCharArray("hello there".toCharArray, 3, 9) }
    an [IndexOutOfBoundsException] must be thrownBy { WrappedCharArray("hello there".toCharArray, 0, Int.MaxValue) }
  }

  test("Extracting an iterator works") {
    val wcai = WrappedCharArray("hello there".toCharArray, 3, 5).iterator
    wcai.next() must be ('l')
    wcai.next() must be ('o')
    wcai.remaining must be (3)
    wcai.freeze.toString must be (" th")
  }
}
