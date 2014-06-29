package com.rojoma.json.v3
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
  def asMeaning: String

  val position: Position
}

sealed abstract class SimpleJsonToken(val asFragment: String, val asMeaning: String) extends JsonToken

case class TokenOpenBrace()(val position: Position) extends SimpleJsonToken("{", "start of object")
case class TokenCloseBrace()(val position: Position) extends SimpleJsonToken("}", "end of object")
case class TokenOpenBracket()(val position: Position) extends SimpleJsonToken("[", "start of list")
case class TokenCloseBracket()(val position: Position) extends SimpleJsonToken("]", "end of list")
case class TokenColon()(val position: Position) extends SimpleJsonToken(":", "colon")
case class TokenComma()(val position: Position) extends SimpleJsonToken(",", "comma")
case class TokenIdentifier(text: String)(val position: Position) extends SimpleJsonToken(text, "identifier")
case class TokenNumber(number: String)(val position: Position) extends JsonToken {
  lazy val asFragment = number
  def asMeaning = "number"
}
case class TokenString(text: String)(val position: Position) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
  def asMeaning = "string"
}
