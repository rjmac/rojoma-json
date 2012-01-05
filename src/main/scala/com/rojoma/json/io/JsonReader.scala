package com.rojoma.json
package io

import java.io.{Reader, StringReader}

import ast._

/** Parses a token-stream into a [[com.rojoma.json.ast.JValue]].  As an extension,
  * this parser accepts unquoted strings (i.e., identifiers) as field-names. */
class JsonReader(input: Iterator[PositionedJsonToken]) {
  val lexer = new JsonEventIterator(input).buffered

  /** Read one JSON datum out of the input.  If the input consists of more than one
    * object, the input after this call is positioned such that the next item available
    * is the first token after the produced object.
    * @return The [[com.rojoma.json.ast.JValue]] read.
    * @throws [[com.rojoma.json.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs. */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def read(): JValue = try {
    val PositionedJsonEvent(event, row, col) = lexer.next()
    event match {
      case StartOfObjectEvent => readObject()
      case StartOfArrayEvent => readArray()
      case StringEvent(s) => JString(s)
      case NumberEvent(n) => JNumber(n)
      case IdentifierEvent("true") => JsonReader.jtrue
      case IdentifierEvent("false") => JsonReader.jfalse
      case IdentifierEvent("null") => JNull
      case IdentifierEvent(other) => throw JsonUnknownIdentifier(other, row, col)
      case EndOfObjectEvent | EndOfArrayEvent | FieldEvent(_) =>
        throw JsonBadParse(event, row, col)
    }
  } catch {
    case NoSuchTokenException(r, c) =>
      throw JsonEOF(r, c)
    case _: NoSuchElementException =>
      throw JsonEOF(-1, -1) // this won't happen with the standard tokenizer and eventer
  }

  private def hopeFor(event: JsonEvent): Boolean = {
    if(event == lexer.head.event) {
      lexer.next()
      true
    } else {
      false
    }
  }

  private def readObject() = {
    // It's bad practice to rely on this, but we'll preserve the order
    // of elements as they're read (barring duplication).
    val result = new scala.collection.mutable.LinkedHashMap[String, JValue]

    var didOne = false
    while(!hopeFor(EndOfObjectEvent)) {
      lexer.next() match {
        case PositionedJsonEvent(FieldEvent(field), _, _) =>
          val value = read()
          result += field -> value
        case PositionedJsonEvent(event, row, col) =>
          throw JsonBadParse(event, row, col)
      }
    }

    JObject(result)
  }

  private def readArray() = {
    val builder = IndexedSeq.newBuilder[JValue]

    var didOne = false
    while(!hopeFor(EndOfArrayEvent)) {
      builder += read()
    }

    JArray(builder.result())
  }
}

object JsonReader {
  def apply(r: Reader): JsonReader = new JsonReader(new TokenIterator(r))
  def apply(s: String): JsonReader = apply(new StringReader(s))

  /** Read a [[com.rojoma.json.ast.JValue]] out of a `Reader`.
    * @param r The source of characters.
    * @return A [[com.rojoma.json.ast.JValue]]
    * @throws [[com.rojoma.json.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs.
    * @see [[com.rojoma.json.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def fromReader(r: Reader) = apply(r).read()

  /** Read a [[com.rojoma.json.ast.JValue]] out of a `String`.
    * @param s The source of characters.
    * @return A [[com.rojoma.json.ast.JValue]]
    * @throws [[com.rojoma.json.io.JsonReaderException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  def fromString(s: String) = apply(s).read()

  private val jtrue = JBoolean(true)
  private val jfalse = JBoolean(false)
}
