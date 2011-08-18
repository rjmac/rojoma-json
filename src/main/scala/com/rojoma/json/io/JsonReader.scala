package com.rojoma.json
package io

import java.io.{Reader, StringReader}

import ast._

/** Parses a token-stream into a [[com.rojoma.json.ast.JValue]].  As an extension,
  * this parser accepts unquoted strings (i.e., identifiers) as field-names. */
class JsonReader(input: Iterator[PositionedJsonToken]) {
  val lexer = input.buffered

  private def expect(want: JsonToken) {
    val n = lexer.next()
    if(n.token != want) throw JsonUnexpectedToken(n.token, want.asMeaning, n.row, n.column)
  }

  private def hopeFor(want: JsonToken) = {
    val PositionedJsonToken(found, row, col) = lexer.head
    if(found == want) {
      lexer.next()
      true
    } else {
      false
    }
  }

  /** Read one JSON datum out of the input.  If the input consists of more than one
    * object, the input after this call is positioned such that the next item available
    * is the first token after the produced object.
    * @return The [[com.rojoma.json.ast.JValue]] read.
    * @throws [[com.rojoma.json.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs. */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def read(): JValue = {
    val PositionedJsonToken(token, row, col) = lexer.next
    token match {
      case TokenOpenBrace => readObject()
      case TokenOpenBracket => readArray()
      case TokenString(s) => JString(s)
      case TokenNumber(n) => JNumber(n)
      case TokenIdentifier("true") => JBoolean(true)
      case TokenIdentifier("false") => JBoolean(false)
      case TokenIdentifier("null") => JNull
      case TokenIdentifier(other) => throw JsonUnknownIdentifier(other, row, col)
      case other => throw JsonUnexpectedToken(other, "start of datum", row, col)
    }
  }

  private def readObject() = {
    // It's bad practice to rely on this, but we'll preserve the order
    // of elements as they're read (barring duplication).
    val result = new scala.collection.mutable.LinkedHashMap[String, JValue]

    var didOne = false
    while(!hopeFor(TokenCloseBrace)) {
      if(didOne) expect(TokenComma)
      else didOne = true

      def restOfField(field: String) {
        expect(TokenColon)
        val v = read()
        result += (field -> v)
      }

      val PositionedJsonToken(token, row, col) = lexer.next()
      token match {
        case TokenString(s) => restOfField(s)
        case TokenIdentifier(s) => restOfField(s)
        case other => throw JsonUnexpectedToken(token, "string", row, col)
      }
    }

    JObject(result)
  }

  private def readArray() = {
    val builder = IndexedSeq.newBuilder[JValue]

    var didOne = false
    while(!hopeFor(TokenCloseBracket)) {
      if(didOne) expect(TokenComma)
      else didOne = true

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
}
