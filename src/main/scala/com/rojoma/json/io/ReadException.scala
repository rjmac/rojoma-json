package com.rojoma.json
package io

case class NoSuchTokenException(row: Int, col: Int) extends NoSuchElementException("Empty iterator")

sealed abstract class JsonReaderException(message: String) extends Exception(message) {
  def row: Int
  def col: Int
}

sealed trait JsonLexException
case class JsonUnexpectedCharacter(character: Char, expected: String, row: Int, col: Int) extends JsonReaderException("%d:%d: Expected %s; got character 0x%02x".format(row, col, expected, character)) with JsonLexException
case class JsonNumberOutOfRange(number: String, row: Int, col: Int) extends JsonReaderException("Cannot store in BigDecimal: " + number) with JsonLexException
case class JsonEOF(row: Int, col: Int) extends JsonReaderException("%d:%d: Unexpected end of input") with JsonLexException

sealed trait JsonParseException
case class JsonUnexpectedToken(token: JsonToken, expected: String, row: Int, col: Int) extends JsonReaderException("%d:%d: Expected %s; got token %s".format(row, col, expected, token.asFragment)) with JsonLexException
case class JsonUnknownIdentifier(identifier: String, row: Int, col: Int) extends JsonReaderException("%d:%d: Unknown identifier " + identifier) with JsonParseException

case class JsonBadParse(event: JsonEvent, row: Int, col: Int) extends JsonReaderException("%d:%d: Received unexpected event %s".format(event, row, col))
