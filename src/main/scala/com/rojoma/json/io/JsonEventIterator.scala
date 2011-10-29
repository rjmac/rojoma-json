package com.rojoma.json
package io

sealed abstract class JsonEvent
case object StartOfObjectEvent extends JsonEvent
case object EndOfObjectEvent extends JsonEvent
case object StartOfArrayEvent extends JsonEvent
case object EndOfArrayEvent extends JsonEvent
case class FieldEvent(name: String) extends JsonEvent
case class IdentifierEvent(text: String) extends JsonEvent
case class NumberEvent(number: BigDecimal) extends JsonEvent
case class StringEvent(string: String) extends JsonEvent

case class PositionedJsonEvent(event: JsonEvent, row: Int, column: Int)

class JsonEventIterator(underlying: Iterator[PositionedJsonToken]) extends Iterator[PositionedJsonEvent] {
  private var stack = new scala.collection.mutable.ArrayStack[State]
  stack.push(AwaitingDatum)

  private var available: PositionedJsonEvent = null

  def hasNext = {
    while(available == null && !stack.isEmpty) {
      val token = underlying.next()
      stack.pop.handle(token)
    }
    available != null
  }

  def next() = {
    if(available == null && !hasNext) throw new NoSuchElementException("Empty iterator")
    val result = available
    available = null
    result
  }

  sealed abstract class State {
    def handle(token: PositionedJsonToken)
  }

  private def error(got: PositionedJsonToken, expected: String): Nothing =
    throw JsonUnexpectedToken(got.token, expected, got.row, got.column)

  private def p(token: PositionedJsonToken, ev: JsonEvent) =
    PositionedJsonEvent(ev, token.row, token.column)

  case object AwaitingDatum extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenOpenBrace =>
          stack.push(AwaitingFieldNameOrEndOfObject)
          available = p(token, StartOfObjectEvent)
        case TokenOpenBracket =>
          stack.push(AwaitingEntryOrEndOfArray)
          available = p(token, StartOfArrayEvent)
        case TokenIdentifier(text) =>
          available = p(token, IdentifierEvent(text))
        case TokenNumber(number) =>
          available = p(token, NumberEvent(number))
        case TokenString(string) =>
          available = p(token, StringEvent(string))
        case _ =>
          error(token, "datum")
      }
    }
  }

  case object AwaitingEntryOrEndOfArray extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenOpenBrace =>
          stack.push(AwaitingCommaOrEndOfArray)
          stack.push(AwaitingFieldNameOrEndOfObject)
          available = p(token, StartOfObjectEvent)
        case TokenOpenBracket =>
          stack.push(AwaitingCommaOrEndOfArray)
          stack.push(AwaitingEntryOrEndOfArray)
          available = p(token, StartOfArrayEvent)
        case TokenIdentifier(text) =>
          stack.push(AwaitingCommaOrEndOfArray)
          available = p(token, IdentifierEvent(text))
        case TokenNumber(number) =>
          stack.push(AwaitingCommaOrEndOfArray)
          available = p(token, NumberEvent(number))
        case TokenString(string) =>
          stack.push(AwaitingCommaOrEndOfArray)
          available = p(token, StringEvent(string))
        case TokenCloseBracket =>
          available = p(token, EndOfArrayEvent)
        case _ =>
          error(token, "datum or end of list")
      }
    }
  }

  case object AwaitingCommaOrEndOfArray extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenComma =>
          stack.push(AwaitingCommaOrEndOfArray)
          stack.push(AwaitingDatum)
        case TokenCloseBracket =>
          available = p(token, EndOfArrayEvent)
        case _ =>
          error(token, "comma or end of list")
      }
    }
  }

  case object AwaitingFieldNameOrEndOfObject extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenCloseBrace =>
          available = p(token, EndOfObjectEvent)
        case TokenString(text) =>
          stack.push(AwaitingKVSep)
          available = p(token, FieldEvent(text))
        case TokenIdentifier(text) =>
          stack.push(AwaitingKVSep)
          available = p(token, FieldEvent(text))
        case _ =>
          error(token, "field name or end of object")
      }
    }
  }

  case object AwaitingFieldName extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenString(text) =>
          stack.push(AwaitingKVSep)
          available = p(token, FieldEvent(text))
        case TokenIdentifier(text) =>
          stack.push(AwaitingKVSep)
          available = p(token, FieldEvent(text))
        case _ =>
          error(token, "field name")
      }
    }
  }

  case object AwaitingKVSep extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenColon =>
          stack.push(AwaitingCommaOrEndOfObject)
          stack.push(AwaitingDatum)
        case _ =>
          error(token, "colon")
      }
    }
  }

  case object AwaitingCommaOrEndOfObject extends State {
    def handle(token: PositionedJsonToken) {
      token.token match {
        case TokenComma =>
          stack.push(AwaitingFieldName)
        case TokenCloseBrace =>
          available = p(token, EndOfObjectEvent)
        case _ =>
          error(token, "comma or end of object")
      }
    }
  }
}
