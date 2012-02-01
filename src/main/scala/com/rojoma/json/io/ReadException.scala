package com.rojoma.json
package io

import ast._

private[io] object pos {
  def apply(row: Int, col: Int, restOfMessage: String, restOfMessageArgs: Any*) = {
    val message = restOfMessage.format(restOfMessageArgs:_*)
    if(row == -1 && col == -1) message
    else row + ":" + col + ": " + message
  }
}

case class NoSuchTokenException(row: Int, col: Int) extends NoSuchElementException("Empty iterator")

sealed abstract class JsonReaderException(message: String) extends Exception(message) {
  def row: Int
  def col: Int
}

sealed trait JsonLexException
case class JsonUnexpectedCharacter(character: Char, expected: String, row: Int, col: Int) extends JsonReaderException(pos(row, col, "Expected %s; got character %s", expected, JString(character.toString))) with JsonLexException
case class JsonNumberOutOfRange(number: String, row: Int, col: Int) extends JsonReaderException(pos(row, col, "Cannot store in BigDecimal: %s", number)) with JsonLexException
case class JsonEOF(row: Int, col: Int) extends JsonReaderException(pos(row, col, "Unexpected end of input")) with JsonLexException

sealed trait JsonParseException
case class JsonUnexpectedToken(token: JsonToken, expected: String, row: Int, col: Int) extends JsonReaderException(pos(row, col, "Expected %s; got token %s", expected, token.asFragment)) with JsonLexException
case class JsonUnknownIdentifier(identifier: String, row: Int, col: Int) extends JsonReaderException(pos(row, col, "Unknown identifier %s", JString(identifier))) with JsonParseException

case class JsonBadParse(event: JsonEvent, row: Int, col: Int) extends JsonReaderException(pos(row, col, "Received unexpected event %s", event))
