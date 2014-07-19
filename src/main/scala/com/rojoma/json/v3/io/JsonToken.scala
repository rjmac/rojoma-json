package com.rojoma.json.v3
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
  def asMeaning: String

  val position: Position
  def unpositioned: JsonToken
}

sealed abstract class SimpleJsonToken(val asFragment: String, val asMeaning: String) extends JsonToken

case class TokenOpenBrace()(val position: Position) extends SimpleJsonToken("{", "start of object") {
  def unpositioned = TokenOpenBrace()(Position.Invalid)
}

case class TokenCloseBrace()(val position: Position) extends SimpleJsonToken("}", "end of object") {
  def unpositioned = TokenCloseBrace()(Position.Invalid)
}

case class TokenOpenBracket()(val position: Position) extends SimpleJsonToken("[", "start of array") {
  def unpositioned = TokenOpenBracket()(Position.Invalid)
}

case class TokenCloseBracket()(val position: Position) extends SimpleJsonToken("]", "end of array") {
  def unpositioned = TokenCloseBracket()(Position.Invalid)
}

case class TokenColon()(val position: Position) extends SimpleJsonToken(":", "colon") {
  def unpositioned = TokenColon()(Position.Invalid)
}

case class TokenComma()(val position: Position) extends SimpleJsonToken(",", "comma") {
  def unpositioned = TokenComma()(Position.Invalid)
}

case class TokenIdentifier(text: String)(val position: Position) extends SimpleJsonToken(text, "identifier") {
  def unpositioned = TokenIdentifier(text)(Position.Invalid)
}

case class TokenNumber(number: String)(val position: Position) extends SimpleJsonToken(number, "number") {
  def unpositioned = TokenNumber(number)(Position.Invalid)
}

case class TokenString(text: String)(val position: Position) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
  def asMeaning = "string"
  def unpositioned = TokenString(text)(Position.Invalid)
}
