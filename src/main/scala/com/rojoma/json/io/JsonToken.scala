package com.rojoma.json
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
  def asMeaning: String

  // Since we don't actually have value classes yet, we'll inline this
  // thing manually so they're only actually created if an exception
  // is thrown.  Once we have true value classes, "row" and "column"
  // will go away and these four lines will become just
  //   "var position = Position.Invalid"
  // See also JsonEvent.
  def position = Position(row, column)
  def position_=(position: Position) { row = position.row; column = position.column }
  private[io] var row = Position.Invalid.row
  private[io] var column = Position.Invalid.column
}

sealed abstract class SimpleJsonToken(val asFragment: String, val asMeaning: String) extends JsonToken

case class TokenOpenBrace() extends SimpleJsonToken("{", "start of object")
case class TokenCloseBrace() extends SimpleJsonToken("}", "end of object")
case class TokenOpenBracket() extends SimpleJsonToken("[", "start of list")
case class TokenCloseBracket() extends SimpleJsonToken("]", "end of list")
case class TokenColon() extends SimpleJsonToken(":", "colon")
case class TokenComma() extends SimpleJsonToken(",", "comma")
case class TokenIdentifier(text: String) extends SimpleJsonToken(text, "identifier")
case class TokenNumber(number: BigDecimal) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JNumber(number))
  def asMeaning = "number"
}
case class TokenString(text: String) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
  def asMeaning = "string"
}
