package com.rojoma.json.v3
package io

import java.io.Reader

import scala.collection.mutable.Builder
import scala.collection.immutable.VectorMap
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

  private def throwLexerEOF() =
    throw new JsonLexerEOF(Position(nextCharRow, nextCharCol))

  private def throwParserEOF() =
    throw new JsonParserEOF(Position(nextCharRow, nextCharCol))

  private def atEOF(): Boolean =
    pos == end && !refill()

  private def skipCharNotAtEOF(): Unit = {
    if(block(pos) == '\n') { nextCharRow += 1; nextCharCol = 1 }
    else { nextCharCol += 1 }
    pos += 1
  }

  // An unexpected EOF should throw a lexer exception because we have
  // not read a complete token
  private def peekCharLexer() = {
    if(atEOF()) throwLexerEOF()
    block(pos)
  }

  // An unexpecte EOF should throw a parser exception because we are
  // not inside a token, and therefore we should simulate the clean
  // end of the event stream
  private def peekCharParser() = {
    if(atEOF()) throwParserEOF()
    block(pos)
  }

  private def peekCharNotAtEOF() = block(pos)

  private def nextCharLexer() = {
    val result = peekCharLexer()
    skipCharNotAtEOF()
    result
  }

  private def nextCharNotAtEOF() = {
    val result = block(pos)
    skipCharNotAtEOF()
    result
  }

  private def skipToEndOfLine() = while(!atEOF() && peekCharNotAtEOF() != '\n') skipCharNotAtEOF()

  private def skipBlockComment(): Unit = {
    var last = nextCharLexer()
    while(last != '*' || peekCharLexer() != '/') last = nextCharLexer()
    skipCharNotAtEOF() // skip final '/'
  }

  private def skipComment(): Unit = {
    skipCharNotAtEOF() // skip opening "/"
    peekCharLexer() match {
      case '/' => skipCharNotAtEOF(); skipToEndOfLine()
      case '*' => skipCharNotAtEOF(); skipBlockComment()
      case c => lexerError(c, "/ or *", nextCharRow, nextCharCol)
    }
  }

  @annotation.tailrec
  private def skipWhitespace(): Unit = {
    while(!atEOF() && Character.isWhitespace(peekCharNotAtEOF())) skipCharNotAtEOF()
    if(!atEOF() && peekCharNotAtEOF() == '/') { skipComment(); skipWhitespace() }
  }

  def read(): JValue = readDatum("datum")

  private def readDatum(expected: String): JValue = {
    skipWhitespace()
    peekCharParser() match {
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
    val token = peekCharParser() match {
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
    skipCharNotAtEOF() // skip opening '{'
    skipWhitespace()
    if(peekCharParser() == '}') {
      skipCharNotAtEOF()
      return JObject.empty
    }

    depth += 1

    val result = VectorMap.newBuilder[String, JValue]
    result += readMapping("field name or end of object")
    readRestOfObjectBody(result)

    depth -= 1

    JObject(result.result())
  }

  private def readRestOfObjectBody(result: Builder[(String, JValue), _]): Unit = {
    skipWhitespace()
    while(peekCharParser() != '}') {
      if(peekCharParser() != ',') badToken("comma or end of object")
      skipCharNotAtEOF()
      result += readMapping("field name")
      skipWhitespace()
    }
    skipCharNotAtEOF()
  }

  private def readMapping(expected: String): (String, JValue) = {
    val fieldName = readFieldName(expected)
    skipWhitespace()
    if(peekCharParser() != ':') badToken("colon")
    skipCharNotAtEOF()
    (fieldName, readDatum("datum"))
  }

  private def readFieldName(expected: String): String = {
    skipWhitespace()
    val fieldName = peekCharParser() match {
      case '"' | '\'' => readRawString()
      case c if Character.isUnicodeIdentifierStart(c) => readRawIdentifier()
      case _ => badToken(expected)
    }
    fieldCache(fieldName, depth)
  }

  private def readArray(): JArray = {
    skipCharNotAtEOF() // skip opening '['
    skipWhitespace()
    if(peekCharParser() == ']') {
      skipCharNotAtEOF()
      return JArray.empty
    }

    depth += 1

    val result = Vector.newBuilder[JValue]
    result += readDatum("datum or end of array")
    readRestOfArrayBody(result)

    depth -= 1

    JArray(result.result())
  }

  private def readRestOfArrayBody(result: Builder[JValue, _]): Unit = {
    skipWhitespace()
    while(peekCharParser() != ']') {
      if(peekCharParser() != ',') badToken("comma or end of array")
      skipCharNotAtEOF()
      result += readDatum("datum")
      skipWhitespace()
    }
    skipCharNotAtEOF()
  }

  private def readString(): JString = JString(readRawString())

  private def readRawString(): String = {
    scratch.setLength(0)
    val Boundary = nextCharLexer()
    while(peekCharLexer() != Boundary) {
      readPotentialSurrogatePair(readChar(), Boundary)
    }
    skipCharNotAtEOF() // skip closing quote
    scratch.toString
  }

  private def readPotentialSurrogatePair(c: Char, endOfString: Char): Unit = {
    if(c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE) {
      readSurrogatePair(c, endOfString)
    } else {
      scratch += c
    }
  }

  private def badChar = 0xfffd.toChar

  @annotation.tailrec
  private def readSurrogatePair(c: Char, endOfString: Char): Unit = {
    if(Character.isHighSurrogate(c)) {
      if(peekCharLexer() == endOfString) {
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
    nextCharLexer() match {
      case '\\' => readEscapedCharacter()
      case c => c
    }
  }

  private def readEscapedCharacter(): Char = {
    def ret(c: Char) = { skipCharNotAtEOF(); c }
    peekCharLexer() match {
      case '"' => ret('"')
      case '\'' => ret('\'')
      case '\\' => ret('\\')
      case '/' => ret('/')
      case 'b' => ret('\b')
      case 'f' => ret('\f')
      case 'n' => ret('\n')
      case 'r' => ret('\r')
      case 't' => ret('\t')
      case 'u' => skipCharNotAtEOF(); readUnicodeCharacter()
      case c => lexerError(c, "string escape character", nextCharRow, nextCharCol)
    }
  }

  private def readUnicodeCharacter(): Char = {
    val h1, h2, h3, h4 = readHexDigit()
    ((h1 << 12) | (h2 << 8) | (h3 << 4) | h4).toChar
  }

  private def isDigit(c: Char) = '0' <= c && c <= '9'

  private def readHexDigit(): Int = {
    peekCharLexer() match {
      case c if isDigit(c) =>
        skipCharNotAtEOF()
        c.toInt - '0'.toInt
      case c if 'a' <= c && c <= 'f' =>
        skipCharNotAtEOF()
        10 + c.toInt - 'a'.toInt
      case c if 'A' <= c && c <= 'F' =>
        skipCharNotAtEOF()
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

  private def expectIdentifier(name: String): Unit = {
    val row = nextCharRow
    val col = nextCharCol
    val ident = readRawIdentifier()
    if(ident != name) throw new JsonUnknownIdentifier(ident, Position(row, col))
  }

  private def readRawIdentifier(): String = {
    scratch.setLength(0)
    scratch += nextCharLexer()
    while(!atEOF() && Character.isUnicodeIdentifierPart(peekCharNotAtEOF())) scratch += nextCharNotAtEOF()
    scratch.toString()
  }

  private def readDigit(): Char = {
    if(!isDigit(peekCharLexer())) lexerError(peekCharLexer(), "digit", nextCharRow, nextCharCol)
    nextCharLexer()
  }

  private def readNumber(): JNumber = JNumber.unsafeFromString(readRawNumber())

  private def readRawNumber(): String = {
    // JSON numbers match (a subset of) the language generated by
    // the regular expression:
    //    -?\d+(\.\d+)?([eE][+-]?\d+)?
    // We'll match the whole thing, within the limits of BigDecimal
    scratch.setLength(0)

    val startPos = Position(nextCharRow, nextCharCol)

    if(peekCharLexer() == '-') scratch += nextCharLexer()

    do { scratch += readDigit() } while(!atEOF() && isDigit(peekCharNotAtEOF()))

    val hasFrac = !atEOF() && peekCharNotAtEOF() == '.'
    if(hasFrac) {
      scratch += nextCharNotAtEOF() // skip decimal
      do { scratch += readDigit() } while(!atEOF() && isDigit(peekCharNotAtEOF()))
    }

    val hasExponent = !atEOF() && (peekCharNotAtEOF() == 'e' || peekCharNotAtEOF() == 'E')
    if(hasExponent) {
      scratch += nextCharNotAtEOF() // skip e/E

      if(peekCharLexer() == '-' || peekCharNotAtEOF() == '+') scratch += nextCharNotAtEOF()
      else scratch += '+' // ensure there's always a sign

      val exponentDigitsStart = scratch.length
      do { scratch += readDigit() } while(!atEOF() && isDigit(peekCharNotAtEOF()))

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
