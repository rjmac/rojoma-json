package com.rojoma.json.v3
package io

import java.io.Reader

import scala.collection.mutable
import scala.collection.immutable
import ast._

class FusedBlockJsonReader(input: Reader, fieldCache: FieldCache = IdentityFieldCache, blockSize: Int = 1024) extends JsonReader {
  def this(text: String) = this(new java.io.StringReader(text))
  def this(text: String, fieldCache: FieldCache) = this(new java.io.StringReader(text), fieldCache)

  private [this] val block = new Array[Char](blockSize)
  private [this] var pos = 0
  private [this] var end = 0
  private [this] var nextCharRow = 1
  private [this] var nextCharCol = 1
  private [this] var depth = 0

  private [this] val scratch = new StringBuilder

  private def lexerError(receivedChar: Char, expected: String, row: Int, col: Int): Nothing = {
    throw new JsonUnexpectedCharacter(receivedChar, expected, Position(row, col))
  }

  private def refill(): Boolean =
    input.read(block) match {
      case -1 =>
        false
      case n =>
        pos = 0
        end = n
        true
    }

  private def throwEOF() =
    throw new JsonLexerEOF(Position(nextCharRow, nextCharCol))

  private def atEOF(): Boolean =
    pos == end && !refill()

  private def ensureChars() {
    if(atEOF) throwEOF()
  }

  private def skipChar() {
    nextChar()
  }

  private def peekChar() = {
    ensureChars()
    block(pos)
  }

  private def nextChar() = {
    val result = peekChar()
    pos += 1
    if(result == '\n') { nextCharRow += 1; nextCharCol = 1 }
    else { nextCharCol += 1 }
    result
  }

  private def skipToEndOfLine() = while(!atEOF() && peekChar() != '\n') skipChar()

  private def skipBlockComment() {
    var last = nextChar()
    while(last != '*' || peekChar() != '/') last = nextChar()
    skipChar() // skip final '/'
  }

  private def skipComment() {
    skipChar() // skip opening "/"
    peekChar() match {
      case '/' => skipChar(); skipToEndOfLine()
      case '*' => skipChar(); skipBlockComment()
      case c => lexerError(c, "/ or *", nextCharRow, nextCharCol)
    }
  }

  @annotation.tailrec
  private def skipWhitespace() {
    while(!atEOF() && Character.isWhitespace(peekChar())) skipChar()
    if(!atEOF() && peekChar() == '/') { skipComment(); skipWhitespace() }
  }

  def read(): JValue = readDatum("datum")

  private def readDatum(expected: String): JValue = {
    skipWhitespace()
    peekChar() match {
      case '{' => readObject()
      case '[' => readArray()
      case '"' | '\'' => readString()
      case '-' => readNumber()
      case 't' => readTrue()
      case 'f' => readFalse()
      case 'n' => readNull()
      case c =>
        if(isDigit(c)) readNumber()
        else badToken(expected)
    }
  }

  private def badToken(expected: String): Nothing = {
    val p = Position(nextCharRow, nextCharCol)
    val token = peekChar() match {
      case '{' => TokenOpenBrace()(p)
      case '}' => TokenCloseBrace()(p)
      case '[' => TokenOpenBracket()(p)
      case ']' => TokenCloseBracket()(p)
      case ',' => TokenComma()(p)
      case ':' => TokenColon()(p)
      case '"' | '\'' => TokenString(readRawString())(p)
      case c if isDigit(c) || c == '-' => TokenNumber(readRawNumber())(p)
      case c if Character.isUnicodeIdentifierStart(c) => TokenIdentifier(readRawIdentifier())(p)
      case c => lexerError(c, expected, nextCharRow, nextCharCol)
    }
    throw new JsonUnexpectedToken(token, expected)
  }

  private def readObject(): JObject = {
    depth += 1

    skipChar() // skip opening '{'
    skipWhitespace()
    if(peekChar() == '}') {
      skipChar()
      return JObject.canonicalEmpty
    }

    val result = new mutable.LinkedHashMap[String, JValue]
    result += readMapping("field name or end of object")
    readRestOfObjectBody(result)

    depth -= 1

    JObject(result)
  }

  private def readRestOfObjectBody(result: mutable.LinkedHashMap[String, JValue]) {
    skipWhitespace()
    while(peekChar() != '}') {
      if(peekChar() != ',') badToken("comma or end of object")
      skipChar()
      result += readMapping("field name")
      skipWhitespace()
    }
    skipChar()
  }

  private def readMapping(expected: String): (String, JValue) = {
    val fieldName = readFieldName(expected)
    skipWhitespace()
    if(peekChar() != ':') badToken("colon")
    skipChar()
    (fieldName, readDatum("datum"))
  }

  private def readFieldName(expected: String): String = {
    skipWhitespace()
    val fieldName = peekChar() match {
      case '"' | '\'' => readRawString()
      case c if Character.isUnicodeIdentifierStart(c) => readRawIdentifier()
      case _ => badToken(expected)
    }
    fieldCache(fieldName, depth)
  }

  private def readArray(): JArray = {
    depth += 1

    skipChar() // skip opening '['
    skipWhitespace()
    if(peekChar() == ']') {
      skipChar()
      return JArray.canonicalEmpty
    }

    val result = new immutable.VectorBuilder[JValue]
    result += readDatum("datum or end of array")
    readRestOfArrayBody(result)

    depth -= 1

    JArray(result.result())
  }

  private def readRestOfArrayBody(result: immutable.VectorBuilder[JValue]) {
    skipWhitespace()
    while(peekChar() != ']') {
      if(peekChar() != ',') badToken("comma or end of array")
      skipChar()
      result += readDatum("datum")
      skipWhitespace()
    }
    skipChar()
  }

  private def readString(): JString = JString(readRawString())

  private def readRawString(): String = {
    scratch.setLength(0)
    val Boundary = nextChar()
    while(peekChar() != Boundary) {
      readPotentialSurrogatePair(readChar(), Boundary)
    }
    skipChar() // skip closing quote
    scratch.toString
  }

  private def readPotentialSurrogatePair(c: Char, endOfString: Char) {
    if(c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE) {
      readSurrogatePair(c, endOfString)
    } else {
      scratch += c
    }
  }

  private def badChar = 0xfffd.toChar

  @annotation.tailrec
  private def readSurrogatePair(c: Char, endOfString: Char) {
    if(Character.isHighSurrogate(c)) {
      if(peekChar() == endOfString) {
        scratch += badChar
      } else {
        val potentialSecondHalf = readChar()
        if(Character.isLowSurrogate(potentialSecondHalf)) {
          scratch += c
          scratch += potentialSecondHalf
        } else {
          scratch += badChar
          if(potentialSecondHalf >= Character.MIN_SURROGATE && potentialSecondHalf <= Character.MAX_SURROGATE) {
            readSurrogatePair(potentialSecondHalf, endOfString)
          } else {
            scratch += potentialSecondHalf
          }
        }
      }
    } else {
      scratch += badChar
    }
  }

  private def readChar(): Char = {
    nextChar() match {
      case '\\' => readEscapedCharacter()
      case c => c
    }
  }

  private def readEscapedCharacter(): Char = {
    def ret(c: Char) = { skipChar(); c }
    peekChar() match {
      case '"' => ret('"')
      case '\'' => ret('\'')
      case '\\' => ret('\\')
      case '/' => ret('/')
      case 'b' => ret('\b')
      case 'f' => ret('\f')
      case 'n' => ret('\n')
      case 'r' => ret('\r')
      case 't' => ret('\t')
      case 'u' => skipChar(); readUnicodeCharacter()
      case c => lexerError(c, "string escape character", nextCharRow, nextCharCol)
    }
  }

  private def readUnicodeCharacter(): Char = {
    val h1, h2, h3, h4 = readHexDigit()
    ((h1 << 12) | (h2 << 8) | (h3 << 4) | h4).toChar
  }

  private def isDigit(c: Char) = '0' <= c && c <= '9'

  private def readHexDigit(): Int = {
    peekChar() match {
      case c if isDigit(c) =>
        skipChar()
        c.toInt - '0'.toInt
      case c if 'a' <= c && c <= 'f' =>
        skipChar()
        10 + c.toInt - 'a'.toInt
      case c if 'A' <= c && c <= 'F' =>
        skipChar()
        10 + c.toInt - 'A'.toInt
      case c =>
        lexerError(c, "hex digit", nextCharRow, nextCharCol)
    }
  }

  private def readTrue(): JBoolean = {
    expectIdentifier("true")
    JBoolean.canonicalTrue
  }

  private def readFalse(): JBoolean = {
    expectIdentifier("false")
    JBoolean.canonicalFalse
  }

  private def readNull(): JNull = {
    expectIdentifier("null")
    JNull
  }

  private def expectIdentifier(name: String) {
    val row = nextCharRow
    val col = nextCharCol
    val ident = readRawIdentifier()
    if(ident != name) throw new JsonUnknownIdentifier(ident, Position(row, col))
  }

  private def readRawIdentifier(): String = {
    scratch.setLength(0)
    scratch += nextChar()
    while(!atEOF() && Character.isUnicodeIdentifierPart(peekChar())) scratch += nextChar()
    scratch.toString()
  }

  private def readDigit(): Char = {
    if(!isDigit(peekChar)) lexerError(peekChar(), "digit", nextCharRow, nextCharCol)
    nextChar()
  }

  private def readNumber(): JNumber = JNumber.unsafeFromString(readRawNumber)

  private def readRawNumber(): String = {
    // JSON numbers match (a subset of) the language generated by
    // the regular expression:
    //    -?\d+(\.\d+)?([eE][+-]?\d+)?
    // We'll match the whole thing, within the limits of BigDecimal
    scratch.setLength(0)

    val startPos = Position(nextCharRow, nextCharCol)

    if(peekChar() == '-') scratch += nextChar()

    do { scratch += readDigit() } while(!atEOF() && isDigit(peekChar()))

    val hasFrac = !atEOF() && peekChar() == '.'
    if(hasFrac) {
      scratch += nextChar() // skip decimal
      do { scratch += readDigit() } while(!atEOF() && isDigit(peekChar()))
    }

    val hasExponent = !atEOF() && (peekChar() == 'e' || peekChar() == 'E')
    if(hasExponent) {
      scratch += nextChar() // skip e/E

      if(peekChar() == '-' || peekChar() == '+') scratch += nextChar()
      else scratch += '+' // ensure there's always a sign

      val exponentDigitsStart = scratch.length
      do { scratch += readDigit() } while(!atEOF() && isDigit(peekChar()))

      // this relies on the exponent being the last thing read
      val result = scratch.toString
      if(!ReaderUtils.isBigDecimalizableUnsignedExponent(result, exponentDigitsStart)) {
        throw new JsonNumberOutOfRange(result, startPos)
      }
      result
    } else {
      scratch.toString
    }
  }
}
