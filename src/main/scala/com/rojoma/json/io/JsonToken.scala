package com.rojoma.json
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
}

sealed abstract class SimpleJsonToken(val asFragment: String) extends JsonToken

case object TokenOpenBrace extends SimpleJsonToken("{")
case object TokenCloseBrace extends SimpleJsonToken("}")
case object TokenOpenBracket extends SimpleJsonToken("[")
case object TokenCloseBracket extends SimpleJsonToken("]")
case object TokenColon extends SimpleJsonToken(":")
case object TokenComma extends SimpleJsonToken(",")
case object TokenEOF extends SimpleJsonToken("end of input")
case class TokenIdentifier(text: String) extends SimpleJsonToken(text)
case class TokenNumber(number: BigDecimal) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JNumber(number))
}
case class TokenString(text: String) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
}

case class PositionedJsonToken(token: JsonToken, row: Int, column: Int)
