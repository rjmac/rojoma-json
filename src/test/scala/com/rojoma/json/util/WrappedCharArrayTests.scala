package com.rojoma.json
package util

import java.nio.CharBuffer

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{Gen, Arbitrary}

class WrappedCharArrayTests extends FunSuite with MustMatchers with PropertyChecks {
  ignore("WrappedCharArray believes it need not copy Strings") {
    WrappedCharArray.canConvertFromStringWithoutCopying must be (true)
  }

  test("WrappedCharArray believes it need not copy when producing a String from String-data") {
    WrappedCharArray.canConvertBackToStringWithoutCopying must be (true)
  }

  test("WrappedCharArray believes it need not copy read-only heap-based CharBuffers") {
    WrappedCharArray.canConvertReadOnlyHeapCharBufferWithoutCopying must be (true)
  }

  test("Converting a String to a WrappedCharArray succeeds") {
    WrappedCharArray("hello there")
  }

  test("Converting a heap-backed CharBuffer to a WrappedCharArray succeeds") {
    WrappedCharArray(CharBuffer.wrap("hello there"))
  }

  test("Converting a read-only heap-backed CharBuffer to a WrappedCharArray succeeds") {
    WrappedCharArray(CharBuffer.wrap("hello there").asReadOnlyBuffer)
  }

  test("Converting a String-based WrappedCharArray back into a String succeeds") {
    WrappedCharArray("hello there").toString must equal ("hello there")
  }

  test("Converting a read-only-heap-backed-CharBuffer-based WrappedCharArray back to a CharBuffer succeeds") {
    // can't use "must be ('readOnly)" because the dynamic type of the class is inaccessible.
    WrappedCharArray(CharBuffer.wrap("hello there".toCharArray).asReadOnlyBuffer).toCharBuffer.isReadOnly must be (true)
    WrappedCharArray(CharBuffer.wrap("hello there")).toCharBuffer.isReadOnly must be (true)
  }

  test("Converting a String-based WrappedCharArray to a CharBuffer succeeds") {
    WrappedCharArray("hello there").toCharBuffer.isReadOnly must be (true)
  }

  test("Converting an array-based WrappedCharArray to a CharBuffer succeeds") {
    WrappedCharArray("hello there".toCharArray).toCharBuffer.isReadOnly must be (false)
  }

  test("Converting a non-read-only-heap-backed-CharBuffer-based WrappedCharArray to a CharBuffer succeeds") {
    WrappedCharArray(CharBuffer.wrap("hello there".toCharArray)).toCharBuffer.isReadOnly must be (false)
  }

  test("Extracting a slice of a char array works") {
    val wca = WrappedCharArray("hello there".toCharArray, 3, 5)
    wca.length must be (5)
    wca(0) must be ('l')
    wca(4) must be ('h')
    wca.toString must be ("lo th")
  }

  test("Wrapping a substring works") {
    val wca = WrappedCharArray("hello there".substring(3, 8))
    wca.length must be (5)
    wca(0) must be ('l')
    wca(4) must be ('h')
    wca.toString must be ("lo th")
  }

  test("Bounds-checking on a slice works") {
    evaluating { WrappedCharArray("hello there".toCharArray, -1, 5) } must produce[IndexOutOfBoundsException]
    evaluating { WrappedCharArray("hello there".toCharArray, 3, -1) } must produce[IndexOutOfBoundsException]
    evaluating { WrappedCharArray("hello there".toCharArray, 3, 9) } must produce[IndexOutOfBoundsException]
    evaluating { WrappedCharArray("hello there".toCharArray, 0, Int.MaxValue) } must produce[IndexOutOfBoundsException]
  }

  test("Extracting an iterator works") {
    val wcai = WrappedCharArray("hello there".toCharArray, 3, 5).iterator
    wcai.next() must be ('l')
    wcai.next() must be ('o')
    wcai.remaining must be (3)
    wcai.freeze.toString must be (" th")
  }
}
