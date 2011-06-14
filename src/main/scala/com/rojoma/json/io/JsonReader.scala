package com.rojoma.json
package io

import java.io.{Reader, StringReader}

import ast._

sealed abstract class JsonParseException(message: String) extends Exception(message)
case class JsonEOF() extends JsonParseException("Unexpected end of input")
case class JsonUnexpectedCharacter(character: Char, expected: String) extends JsonParseException("Expected " + expected + "; got character " + character.toInt)
case class JsonUnknownIdentifier(identifier: String) extends JsonParseException("Unknown identifier " + identifier)
case class JsonNumberOutOfRange(number: String) extends JsonParseException("Cannot store in BigDecimal: " + number)

/** Parses a character-stream into a [[com.rojoma.json.ast.JValue]].
  * 
  * This is guaranteed to read no more than necessary to ensure it
  * has reached the end of the object.  For objects, arrays, and strings,
  * it will read only up to the closing delimiter.  For other types, it
  * may read one character further to assure itself that it has reached
  * the end.
  *
  * This class does many small reads; it may be a good idea to wrap
  * the input `Reader` into a `BufferedReader`.
  *
  * As extensions, this reader supports single-quoted strings as well
  * as unquoted field-names. */
class JsonReader(r: Reader) {
  private var isPeeked: Boolean = false
  private var peeked: Char = _

  private def next() = {
    peek()
    isPeeked = false
    peeked
  }

  private def peek() = {
    if(!isPeeked) {
      val newChar = r.read()
      if(newChar == -1) throw JsonEOF()
      peeked = newChar.toChar
      isPeeked = true
    }
    peeked
  }

  private def atEOF(): Boolean = {
    if(isPeeked) return false
    val newChar = r.read()
    if(r == -1) return true
    peeked = newChar.toChar
    isPeeked = true
    return false
  }

  private def skipToEndOfLine() = while(peek() != '\n') next()

  private def skipBlockComment() {
    var last = next()
    while(last != '*' || peek() != '/') last = next()
    next() // skip final '/'
  }

  private def skipComment() {
    next() // skip opening "/"
    next() match {
      case '/' => skipToEndOfLine()
      case '*' => skipBlockComment()
      case c => throw JsonUnexpectedCharacter(c, "/ or *")
    }
  }

  private def skipWhitespace() {
    while(Character.isWhitespace(peek())) next()
    if(peek() == '/') { skipComment(); skipWhitespace() }
  }

  private def expect(c: Char) {
    skipWhitespace()
    val n = next()
    if(n != c) throw JsonUnexpectedCharacter(n, c.toString)
  }

  private def hopeFor(c: Char) = {
    skipWhitespace()
    if(peek() == c) {
      next()
      true
    } else {
      false
    }
  }

  /** Read one JSON datum out of the `Reader`.
    * @return The [[com.rojoma.json.ast.JValue]] read.
    * @throws [[com.rojoma.json.io.JsonParseException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs. */
  @throws(classOf[JsonParseException])
  @throws(classOf[java.io.IOException])
  def read(): JValue = {
    skipWhitespace()
    peek() match {
      case '{' => readObject()
      case '[' => readArray()
      case '"' | '\'' => readString()
      case '-' => readNumber()
      case other => other match { // nested case to allow scalac to emit a lookupswitch instruction for the above options
        case c if isDigit(c) => readNumber()
        case c if Character.isUnicodeIdentifierStart(c) => parseIdentifier(readIdentifier())
        case c => throw JsonUnexpectedCharacter(c, "start of datum")
      }
    }
  }

  private def parseIdentifier(id: String) = id match {
    case "true" => JBoolean(true)
    case "false" => JBoolean(false)
    case "null" => JNull
    case other => throw JsonUnknownIdentifier(other)
  }

  private def readObject() = {
    // It's bad practice to rely on this, but we'll preserve the order
    // of elements as they're read (barring duplication).
    val result = new scala.collection.mutable.LinkedHashMap[String, JValue]
    next() // skip '{'

    var didOne = false
    while(!hopeFor('}')) {
      if(didOne) {
        expect(',')
      } else {
        didOne = true
      }

      skipWhitespace()

      def readRestOfField(field: String) {
        expect(':')
        val v = read()
        result += (field -> v)
      }

      if(peek() == '"' | peek() == '\'') {
        val JString(k) = readString()
        readRestOfField(k)
      } else if(Character.isUnicodeIdentifierStart(peek())) {
        readRestOfField(readIdentifier())
      }
    }

    JObject(result)
  }

  private def readArray() = {
    val builder = IndexedSeq.newBuilder[JValue]
    next() // skip '['

    var didOne = false
    while(!hopeFor(']')) {
      if(didOne) {
        expect(',')
      } else {
        didOne = true
      }

      skipWhitespace()

      builder += read()
    }

    JArray(builder.result())
  }

  private def readString() = {
    val sb = new StringBuilder
    val Boundary = next()
    while(peek() != Boundary) {
      readPotentialSurrogatePairInto(sb, readChar(), Boundary)
    }
    next() // skip closing character
    JString(sb.toString)
  }

  @annotation.tailrec
  private def readPotentialSurrogatePairInto(sb: StringBuilder, c: Char, endOfString: Char) {
    if(c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE) {
      val badChar = 0xfffd.toChar
      if(Character.isHighSurrogate(c)) {
        if(peek() == endOfString) {
          sb += badChar
        } else {
          val potentialSecondHalf = readChar()
          if(Character.isLowSurrogate(potentialSecondHalf)) {
            sb += c
            sb += potentialSecondHalf
          } else {
            sb += badChar
            readPotentialSurrogatePairInto(sb, potentialSecondHalf, endOfString)
          }
        }
      } else {
        sb += badChar
      }
    } else {
      sb += c
    }
  }

  private def readChar() = {
    next() match {
      case '\\' => readEscapedCharacter()
      case c => c
    }
  }

  private def readEscapedCharacter(): Char = {
    next() match {
      case '"' => '"'
      case '\'' => '\''
      case '\\' => '\\'
      case '/' => '/'
      case 'b' => '\b'
      case 'f' => '\f'
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case 'u' => readUnicodeCharacter()
      case c => throw JsonUnexpectedCharacter(c, "string escape character")
    }
  }

  private def readUnicodeCharacter(): Char = {
    val h1, h2, h3, h4 = readHexDigit()
    ((h1 << 12) | (h2 << 8) | (h3 << 4) | h4).toChar
  }

  private def isDigit(c: Char) = '0' <= c && c <= '9'

  private def readHexDigit(): Int = {
    val c = next()
    c match {
      case c if isDigit(c) => c.toInt - '0'.toInt
      case c if 'a' <= c && c <= 'f' => 10 + c.toInt - 'a'.toInt
      case c if 'A' <= c && c <= 'F' => 10 + c.toInt - 'A'.toInt
      case c => throw JsonUnexpectedCharacter(c, "hex digit")
    }
  }

  private def readIdentifier() = {
    val sb = new StringBuilder
    sb += next()
    while(!atEOF() && Character.isUnicodeIdentifierPart(peek())) sb += next()
    sb.toString()
  }

  private def readDigit() = {
    val c = next()
    if(!isDigit(c)) throw JsonUnexpectedCharacter(c, "digit")
    c
  }

  private def readNumber(): JNumber = {
    // JSON numbers match (a subset of) the language generated by
    // the regular expression:
    //    -?\d+(\.\d+)?([eE][+-]?\d+)?
    // We'll match the whole thing, within the limits of a 64-bit
    // IEEE processor.
    val sb = new StringBuilder

    if(peek() == '-') sb += next()

    do { sb += readDigit() } while(!atEOF() && isDigit(peek()))

    val hasFrac = !atEOF() && peek() == '.'
    if(hasFrac) {
      sb += next() // skip decimal
      do { sb += readDigit() } while(!atEOF() && isDigit(peek()))
    }

    val hasExponent = !atEOF() && (peek() == 'e' || peek() == 'E')
    if(hasExponent) {
      sb += next() // skip e/E
      if(peek() == '-') sb += next()
      else if(peek() == '+') next() // just skip it
      do { sb += readDigit() } while(!atEOF() && isDigit(peek()))
    }

    val number = sb.toString

    try {
      JNumber(BigDecimal(number, java.math.MathContext.UNLIMITED))
    } catch {
      case _: NumberFormatException =>
        throw JsonNumberOutOfRange(number)
    }
  }
}

object JsonReader {
  /** Read a [[com.rojoma.json.ast.JValue]] out of a `Reader`.
    * @param r The source of characters.
    * @return A [[com.rojoma.json.ast.JValue]]
    * @throws [[com.rojoma.json.io.JsonParseException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs.
    * @see [[com.rojoma.json.io.JsonReader]] */
  @throws(classOf[JsonParseException])
  @throws(classOf[java.io.IOException])
  def fromReader(r: Reader) = new JsonReader(r).read

  /** Read a [[com.rojoma.json.ast.JValue]] out of a `String`.
    * @param s The source of characters.
    * @return A [[com.rojoma.json.ast.JValue]]
    * @throws [[com.rojoma.json.io.JsonParseException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.io.JsonReader]] */
  @throws(classOf[JsonParseException])
  def fromString(s: String) = fromReader(new StringReader(s))
}
