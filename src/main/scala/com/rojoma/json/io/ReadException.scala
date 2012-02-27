package com.rojoma.json
package io

import ast._

private[io] object pos {
  def apply(row: Int, column: Int, restOfMessage: String, restOfMessageArgs: Any*) = {
    val message = restOfMessage.format(restOfMessageArgs:_*)
    if(row == -1 && column == -1) message
    else row + ":" + column + ": " + message
  }
}

case class NoSuchTokenException(row: Int, column: Int) extends NoSuchElementException("Empty iterator")

sealed abstract class JsonReaderException(message: String) extends Exception(message) {
  def row: Int
  def column: Int
}

sealed trait JsonLexException
case class JsonUnexpectedCharacter(character: Char, expected: String, row: Int, column: Int) extends JsonReaderException(pos(row, column, "Expected %s; got character %s", expected, JString(character.toString))) with JsonLexException
case class JsonNumberOutOfRange(number: String, row: Int, column: Int) extends JsonReaderException(pos(row, column, "Cannot store in BigDecimal: %s", number)) with JsonLexException
case class JsonEOF(row: Int, column: Int) extends JsonReaderException(pos(row, column, "Unexpected end of input")) with JsonLexException

sealed trait JsonParseException
case class JsonUnexpectedToken(token: JsonToken, expected: String) extends JsonReaderException(pos(token.row, token.column, "Expected %s; got token %s", expected, token.asFragment)) with JsonLexException {
  def row = token.row
  def column = token.column
}
case class JsonUnknownIdentifier(identifier: String, row: Int, column: Int) extends JsonReaderException(pos(row, column, "Unknown identifier %s", JString(identifier))) with JsonParseException

case class JsonBadParse(event: JsonEvent, row: Int, column: Int) extends JsonReaderException(pos(row, column, "Received unexpected event %s", event))
