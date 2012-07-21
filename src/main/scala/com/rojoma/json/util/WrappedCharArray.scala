package com.rojoma.json
package util

import java.lang.reflect.{Array => _, _}

/** A container for a slice of an `Array[Char]` which promises to allow only read-only
 * access to that array.  Note it does not itself copy the array, so if there is another
 * reference the data can be mutated by other operations.  This is used for copyless
 * lexing of data contained in Strings. */
final class WrappedCharArray private [util] (chars: Array[Char], offset: Int, count: Int, fromString: Boolean) {
  def isEmpty = count == 0

  def length = count

  /** @return a copy of the slice of the underlying array. */
  def toCharArray = java.util.Arrays.copyOfRange(chars, offset, count)

  /** @return The underlying array-slice as a String.  If the data
   * was originally from a String, this tries not to make a copy. */
  override def toString =
    if(fromString) WrappedCharArray.unsafeConstructString(chars, offset, count)
    else new String(chars, offset, count)

  def apply(i: Int): Char = {
    if(i < 0) throw new IndexOutOfBoundsException("i < 0")
    if(i >= count) throw new IndexOutOfBoundsException("i >= count")
    chars(offset + i)
  }

  /** Returns a BufferedIterator with two extra methods: remaining, which
   * returns the number of characters left to consume, and "freeze". which
   * produces a WrappedCharArray containing the remaining characters without
   * copying. */
  def iterator = new WrappedCharArrayIterator(chars, offset, offset + count, fromString)
}

object WrappedCharArray {
  def apply(chars: Array[Char], offset: Int, count: Int): WrappedCharArray = {
    if(offset < 0) throw new IndexOutOfBoundsException("offset < 0")
    if(count < 0) throw new IndexOutOfBoundsException("count < 0")
    if(offset > chars.length - count) throw new IndexOutOfBoundsException("offset + count > length")
    new WrappedCharArray(chars, offset, count, false)
  }

  def apply(chars: Array[Char]): WrappedCharArray = new WrappedCharArray(chars, 0, chars.length, false)

  def apply(chars: String): WrappedCharArray = unsafeRewrapString(chars)

  private class RewrapStringFunc(valueField: Field, offsetField: Field, countField: Field) extends Function1[String, WrappedCharArray] {
    def apply(s: String) =
      new WrappedCharArray(valueField.get(s).asInstanceOf[Array[Char]], offsetField.getInt(s), countField.getInt(s), true)
  }

  private val unsafeRewrapString = try {
    val strCls = classOf[String]
    val valueField = strCls.getDeclaredField("value")
    val offsetField = strCls.getDeclaredField("offset")
    val countField = strCls.getDeclaredField("count")
    valueField.setAccessible(true)
    offsetField.setAccessible(true)
    countField.setAccessible(true)
    new RewrapStringFunc(valueField, offsetField, countField)
  } catch {
    case _: NoSuchFieldException | _: SecurityException =>
      { (s: String) =>
        val arr = s.toCharArray
        new WrappedCharArray(arr, 0, arr.length, true)
      }
  }

  val canConvertFromStringWithoutCopying = unsafeRewrapString.isInstanceOf[RewrapStringFunc]

  private class ConstructStringFunc(ctor: Constructor[String]) extends Function3[Array[Char], Int, Int, String] {
    def apply(c: Array[Char], offset: Int, length: Int) =
      ctor.newInstance(offset : java.lang.Integer, length : java.lang.Integer, c)
  }

  private val unsafeConstructString = try {
    val ctor = classOf[String].getDeclaredConstructor(classOf[Int], classOf[Int], classOf[Array[Char]])
    ctor.setAccessible(true)
    new ConstructStringFunc(ctor)
  } catch {
    case _: NoSuchMethodException | _: SecurityException =>
      (c: Array[Char], offset: Int, length: Int) => new String(c, offset, length)
  }

  val canConvertBackToStringWithoutCopying = unsafeConstructString.isInstanceOf[ConstructStringFunc]

  val empty = apply("")
}

class WrappedCharArrayIterator private[util] (chars: Array[Char], private[this] var offset: Int, limit: Int, fromString: Boolean) extends BufferedIterator[Char] {
  def hasNext = offset != limit

  def next() = {
    val result = head
    offset += 1
    result
  }

  def head = {
    if(offset == limit) throw new NoSuchElementException("Read past end of data")
    chars(offset)
  }

  def remaining = limit - offset

  def freeze = new WrappedCharArray(chars, offset, limit - offset, fromString)
}
