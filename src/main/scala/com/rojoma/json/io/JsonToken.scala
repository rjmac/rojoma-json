package com.rojoma.json
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
  def asMeaning: String
}

sealed abstract class SimpleJsonToken(val asFragment: String, val asMeaning: String) extends JsonToken

case object TokenOpenBrace extends SimpleJsonToken("{", "start of object")
case object TokenCloseBrace extends SimpleJsonToken("}", "end of object")
case object TokenOpenBracket extends SimpleJsonToken("[", "start of list")
case object TokenCloseBracket extends SimpleJsonToken("]", "end of list")
case object TokenColon extends SimpleJsonToken(":", "colon")
case object TokenComma extends SimpleJsonToken(",", "comma")
case object TokenEOF extends SimpleJsonToken("end of input", "end of input")
case class TokenIdentifier(text: String) extends SimpleJsonToken(text, "identifier")
case class TokenNumber(number: BigDecimal) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JNumber(number))
  def asMeaning = "number"
}
case class TokenString(text: String) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
  def asMeaning = "string"
}

case class PositionedJsonToken(token: JsonToken, row: Int, column: Int)
