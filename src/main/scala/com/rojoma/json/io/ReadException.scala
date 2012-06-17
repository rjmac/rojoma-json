package com.rojoma.json
package io

import ast._

private[io] object pos {
  def apply(position: Position, restOfMessage: String, restOfMessageArgs: Any*) = {
    val message = restOfMessage.format(restOfMessageArgs:_*)
    if(position.row == -1 && position.column == -1) message
    else position + ": " + message
  }
}

class NoSuchTokenException(val position: Position) extends NoSuchElementException("Empty iterator") {
  def row: Int = position.row
  def column: Int = position.column
}

abstract class JsonReaderException(val message: String) extends Exception(message) {
  def position: Position
  def row: Int = position.row
  def column: Int = position.column
}

trait JsonLexException extends JsonReaderException
trait JsonParseException extends JsonReaderException
trait JsonReadException extends JsonReaderException

class JsonUnexpectedCharacter(val character: Char, val expected: String, val position: Position) extends JsonReaderException(pos(position, "Expected %s; got character %s", expected, JString(character.toString))) with JsonLexException
class JsonNumberOutOfRange(val number: String, val position: Position) extends JsonReaderException(pos(position, "Cannot store in BigDecimal: %s", number)) with JsonLexException

sealed abstract class JsonEOF(val position: Position) extends JsonReaderException(pos(position, "Unexpected end of input"))
class JsonLexerEOF(position: Position) extends JsonEOF(position) with JsonLexException
class JsonParserEOF(position: Position) extends JsonEOF(position) with JsonParseException

class JsonUnexpectedToken(val token: JsonToken, val expected: String) extends JsonReaderException(pos(token.position, "Expected %s; got token %s", expected, token.asFragment)) with JsonParseException {
  def position = token.position
}
class JsonUnknownIdentifier(val identifier: String, val position: Position) extends JsonReaderException(pos(position, "Unknown identifier %s", JString(identifier))) with JsonParseException {
  def this(i: IdentifierEvent) = this(i.text, i.position)
}

/** This exception should never be thrown if using the standard
 * `JsonEventIterator`.  It means that either there were mismatched
 * start/end array or object events or that an an object did not
 * follow the pattern of FieldObject-followed-by-field-data. */
class JsonBadParse(val event: JsonEvent) extends JsonReaderException(pos(event.position, "Received unexpected event %s", event)) with JsonReadException {
  def position = event.position
}
