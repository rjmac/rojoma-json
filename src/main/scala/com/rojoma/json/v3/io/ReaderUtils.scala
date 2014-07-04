package com.rojoma.json.v3
package io

private[io] object ReaderUtils {
  private val intMaxLength = Int.MaxValue.toString.length

  // finds the exponent, if there is one, then returns isBigDecimalizableExponent
  // from there.  Requires the number be well-formed
  def isBigDecimalizable(s: String): Boolean = {
    var i = 0
    val len = s.length
    while(i != len) {
      val c = s.charAt(i)
      if(c == 'e' || c == 'E') return isBigDecimalizableExponent(s, i + 1)
      i += 1
    }
    true
  }

  // Ensures that the absolute value of the number represented by this string
  // starting at this offset fits in 31 bits.
  def isBigDecimalizableExponent(s: String, offset: Int = 0): Boolean = {
    def isSigned = {
      val c = s.charAt(offset)
      c == '+' || c == '-'
    }
    if(isSigned) isBigDecimalizableUnsignedExponent(s, offset + 1)
    else isBigDecimalizableUnsignedExponent(s, offset)
  }

  def isBigDecimalizableUnsignedExponent(s: String, offset: Int) = {
    val lenInChars = s.length - offset
    lenInChars < intMaxLength || (lenInChars == intMaxLength && s.toLong <= Int.MaxValue)
  }
}
