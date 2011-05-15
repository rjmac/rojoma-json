package json
package io

import java.io.{Reader, StringReader}

import ast._

case class JsonParseException(message: String) extends Exception(message)

/** Parses a character-stream into a [[json.ast.JValue]].
  * 
  * This is guaranteed to read no more than necessary to ensure it
  * has reached the end of the object.  For objects, arrays, and strings,
  * it will read only up to the closing delimiter.  For other types, it
  * may read one character further to assure itself that it has reached
  * the end.
  *
  * This class does many small reads; it may be a good idea to wrap
  * the input `Reader` into a `BufferedReader`. */
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
      if(newChar == -1) throw new JsonParseException("Unexpected end of input")
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

  private def skipWhitespace() = while(Character.isWhitespace(peek())) next()

  private def expect(c: Char) {
    skipWhitespace()
    val n = next()
    if(n != c) throw JsonParseException("Expected \"" + c + "\"; got " + n.toInt)
    skipWhitespace()
  }

  /** Read one JSON datum out of the `Reader`.
    * @return The [[json.ast.JValue]] read.
    * @throws [[json.io.JsonParseException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs. */
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
        case _ => throw JsonParseException("Unexpected character at start of datum: " + peek().toInt)
      }
    }
  }

  private def parseIdentifier(id: String) = id match {
    case "true" => JBoolean(true)
    case "false" => JBoolean(false)
    case "null" => JNull
    case _ => throw JsonParseException("Unknown identifier " + id)
  }

  private def readObject(): JObject = {
    // It's bad practice to rely on this, but we'll preserve the order
    // of elements as they're read (barring duplication).
    val result = new scala.collection.mutable.LinkedHashMap[String, JValue]
    next() // skip '{'

    var didOne = false
    while(true) {
      skipWhitespace()
      if(peek() == '}') {
        next() // skip it
        return JObject(result)
      }

      if(didOne) {
        expect(',')
      } else {
        didOne = true
      }

      if(peek() == '"' | peek() == '\'') {
        val JString(k) = readString()
        expect(':')
        val v = read()
        result += (k -> v)
      }
    }
    error("Can't get here")
  }

  private def readArray(): JArray = {
    val builder = IndexedSeq.newBuilder[JValue]
    next() // skip '['

    var didOne = false
    while(true) {
      skipWhitespace()
      if(peek() == ']') {
        next() // skip it
        return JArray(builder.result())
      }

      if(didOne) {
        expect(',')
      } else {
        didOne = true
      }

      builder += read()
    }
    error("Can't get here")
  }

  private def readString() = { // TODO: validate surrogate pairs
    val sb = new StringBuilder
    val Boundary = next()
    while(peek() != Boundary) {
      next() match {
        case '\\' => sb += readEscapedCharacter()
        case c => sb += c
      }
    }
    next() // skip closing character
    JString(sb.toString)
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
      case c => throw JsonParseException("Unknown string escape " + c.toInt)
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
      case c => throw JsonParseException("Expected hex digit; got " + c.toInt)
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
    if(!isDigit(c)) throw JsonParseException("Expected digit; got " + c.toInt)
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

    if(hasFrac || hasExponent) { // floating point!
      JNumber(sb.toString.toDouble)
    } else {
      JNumber(sb.toString.toLong)
    }
  }
}

object JsonReader {
  /** Read a [[json.ast.JValue]] out of a `Reader`.
    * @param r The source of characters.
    * @return A [[json.ast.JValue]]
    * @throws [[json.io.JsonParseException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs.
    * @see [[json.io.JsonReader]] */
  @throws(classOf[JsonParseException])
  @throws(classOf[java.io.IOException])
  def fromReader(r: Reader) = new JsonReader(r).read

  /** Read a [[json.ast.JValue]] out of a `String`.
    * @param s The source of characters.
    * @return A [[json.ast.JValue]]
    * @throws [[json.io.JsonParseException]] if a complete object cannot be read.
    * @see [[json.io.JsonReader]] */
  @throws(classOf[JsonParseException])
  def fromString(s: String) = fromReader(new StringReader(s))
}
