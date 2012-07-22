package com.rojoma.json
package util

import java.lang.reflect.{Array => _, _}
import java.nio.CharBuffer

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
   * was originally from a String and `WrappedCharArray.canConvertBackToStringWithoutCopying`
   * is true, this will not involve a copy. */
  override def toString =
    if(fromString) WrappedCharArray.unsafeConstructString(chars, offset, count)
    else new String(chars, offset, count)

  /** Convert this `WrappedCharArray` into a `CharBuffer`.  If the data originally came
   * from a String, the result will be read-only.  In neither case is the data copied.
   *
   * @return The underlying array-slice as a CharBuffer. */
  def toCharBuffer: CharBuffer = {
    val cb = CharBuffer.wrap(chars, offset, count)
    if(fromString) cb.asReadOnlyBuffer
    else cb
  }

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

  override def equals(o: Any) = o match {
    case that: WrappedCharArray =>
      (this eq that) || that.arrayEquals(chars, offset, count)
    case _ => false
  }

  override def hashCode: Int = {
    // Simple hashCode similar to the one performed by java.util.Arrays#hashCode(char[]).
    var result = 1
    var i = offset
    var end = offset + count
    while(i != end) {
      result = 31*result + chars(i)
      i += 1
    }
    result
  }

  private def arrayEquals(thoseChars: Array[Char], thatOffset: Int, thatCount: Int): Boolean = {
    if(count != thatCount) return false
    var i = 0
    while(i != count) {
      if(chars(i + offset) != thoseChars(i + thatOffset)) return false
      i += 1
    }
    return true;
  }
}

object WrappedCharArray {
  /**
   * Convert a slice of an array into a `WrappedCharArray`.  The new object will be backed by the
   * array, and so changes to the array's contents will be reflected in the resulting `WrappedCharArray`.
   *
   * @throws IndexOutOfBoundsException
   */
  def apply(chars: Array[Char], offset: Int, count: Int): WrappedCharArray = {
    if(offset < 0) throw new IndexOutOfBoundsException("offset < 0")
    if(count < 0) throw new IndexOutOfBoundsException("count < 0")
    if(offset > chars.length - count) throw new IndexOutOfBoundsException("offset + count > length")
    new WrappedCharArray(chars, offset, count, false)
  }

  /**
   * Convert an entire array into a `WrappedCharArray`.  The new object will be backed by the
   * array, and so changes to the array's contents will be reflected in the resulting `WrappedCharArray`.
   */
  def apply(chars: Array[Char]): WrappedCharArray = new WrappedCharArray(chars, 0, chars.length, false)

  /**
   * Convert a `String` into a `WrappedCharArray`.  If `canConvertFromStringWithoutCopying` is
   * true, this will involve no copying of character data.
   */
  def apply(chars: String): WrappedCharArray = unsafeRewrapString(chars)

  /**
   * Convert a `CharBuffer` into a `WrappedCharArray`.  If the `CharBuffer` has an accessible backing
   * array, this will involve no copying of character data and so changes to that array will be reflected
   * in the resulting `WrappedCharArray`.  Whether or not there is a backing array, after this call the
   * `CharBuffer` has been marked as fully consumed.
   */
  def apply(chars: CharBuffer): WrappedCharArray =
    if(chars.hasArray) {
      val result = new WrappedCharArray(chars.array, chars.arrayOffset + chars.position, chars.remaining, false)
      chars.position(chars.position + chars.remaining) // mark them all as consumed so both branches have the same side-effects
      result
    } else {
      val rawChars = new Array[Char](chars.remaining)
      chars.get(rawChars)
      apply(rawChars)
    }

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

  /** Indicates whether `apply(String)` can avoid copying. */
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

  /** Indicates whether a `WrappedCharArray` that originally came from a `String` can avoid copying
   * in its `toString` method. */
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

  /** @return the number of `Char`s still available */
  def remaining = limit - offset

  /** Produces a new `WrappedCharArray` containing all remaining characters.  After this
   * call, this iterator is still valid and positioned in the same location.
   */
  def freeze = new WrappedCharArray(chars, offset, limit - offset, fromString)
}
