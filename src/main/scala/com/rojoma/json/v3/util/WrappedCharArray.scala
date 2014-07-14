package com.rojoma.json.v3
package util

import `-impl`.util.AbstractBufferedIterator

import java.nio.CharBuffer

/** A container for a slice of an `Array[Char]` which promises to allow only read-only
 * access to that array.  Note it does not itself copy the array, so if there is another
 * reference the data can be mutated by other operations. */
final class WrappedCharArray private [util] (chars: Array[Char], offset: Int, count: Int) {
  def isEmpty = count == 0

  def length = count

  /** @return a copy of the slice of the underlying array. */
  def toCharArray = java.util.Arrays.copyOfRange(chars, offset, count)

  /** @return The underlying array-slice as a String. */
  override def toString = new String(chars, offset, count)

  /** Convert this `WrappedCharArray` into a `CharBuffer`.
   *
   * @return The underlying array-slice as a (read-only) CharBuffer. */
  def toCharBuffer: CharBuffer = CharBuffer.wrap(chars, offset, count).asReadOnlyBuffer

  def apply(i: Int): Char = {
    if(i < 0) throw new IndexOutOfBoundsException("i < 0")
    if(i >= count) throw new IndexOutOfBoundsException("i >= count")
    chars(offset + i)
  }

  /** Returns a BufferedIterator with two extra methods: remaining, which
   * returns the number of characters left to consume, and "freeze". which
   * produces a WrappedCharArray containing the remaining characters without
   * copying. */
  def iterator = new WrappedCharArrayIterator(chars, offset, offset + count)

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
    new WrappedCharArray(chars, offset, count)
  }

  /**
   * Convert an entire array into a `WrappedCharArray`.  The new object will be backed by the
   * array, and so changes to the array's contents will be reflected in the resulting `WrappedCharArray`.
   */
  def apply(chars: Array[Char]): WrappedCharArray = new WrappedCharArray(chars, 0, chars.length)

  def fromString(chars: String) = apply(chars.toCharArray)

  val empty = apply(new Array[Char](0))
}

class WrappedCharArrayIterator private[util] (chars: Array[Char], private[this] var offset: Int, limit: Int) extends AbstractBufferedIterator[Char] {
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
  def freeze = new WrappedCharArray(chars, offset, limit - offset)
}
