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
    val isNeg = s.charAt(offset-1) == '-'
    lenInChars < intMaxLength || (lenInChars == intMaxLength && (if(isNeg) {
                                                                   s.substring(offset).toLong <= (Int.MaxValue+1L)
                                                                 } else {
                                                                   s.substring(offset).toLong <= Int.MaxValue
                                                                 }))
  }

  def isValidIdentifier(s: String): Boolean = {
    exactlyOneToken(s) match {
      case Some(TokenIdentifier(_)) => true
      case _ => false
    }
  }

  def isValidNumber(s: String): Boolean = {
    exactlyOneToken(s) match {
      case Some(TokenNumber(_)) => true
      case _ => false
    }
  }

  private def exactlyOneToken(n: String): Option[JsonToken] =
    JsonTokenGenerator.newGenerator(n) match {
      case JsonTokenGenerator.Token(t, _, ri) if ri.isEmpty => Some(t)
      case JsonTokenGenerator.More(cont) =>
        cont.endOfInput() match {
          case JsonTokenGenerator.FinalToken(t, _) => Some(t)
          case _ => None
        }
      case _ => None
    }
}
