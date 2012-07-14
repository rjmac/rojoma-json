package com.rojoma.json
package io

import JsonParser._

class JsonParser private (val stack: ::[State]) /* extends AnyVal in 2.10? */ {
  // That type for "stack" makes the state-transition code down below a little awkward
  // since "x :: xs" infers to List, not ::.  So in order to get the types done right,
  // it has to be written as "new ::(x, xs)" which has exactly the same meaning but
  // the right type.
  def apply(token: PositionedJsonToken): ParseResult =
    stack.head.handle(token, stack.tail)

  def parse(token: PositionedJsonToken): SuccessfulParseResult = apply(token) match {
    case r: SuccessfulParseResult => r
    case ParseError(token, expected, row, col) => throw new JsonUnexpectedToken(token, expected, row, col)
  }

  def atTopLevel: Boolean = stack eq newParser.stack

  override def equals(o: Any): Boolean = o match {
    case that: JsonParser => this.stack == that.stack
    case _ => false
  }

  override def hashCode: Int = stack.hashCode

  override def toString = stack.mkString("JsonParser(", ",", ")")
}

object JsonParser {
  sealed abstract class State {
    protected def event(token: PositionedJsonToken, ev: JsonEvent, stack: List[State]): ParseResult = {
      val pEv = PositionedJsonEvent(ev, token.row, token.column)
      stack match {
        case cons@(_ :: _) => Event(pEv, new JsonParser(cons))
        case _ => Event(pEv, newParser)
      }
    }

    protected def more(stack: ::[State]): ParseResult =
      More(new JsonParser(stack))

    protected def error(got: PositionedJsonToken, expected: String): ParseResult =
      ParseError(got.token, expected, got.row, got.column)

    def handle(token: PositionedJsonToken, stack: List[State]): ParseResult
  }

  private val AwaitingDatum: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenOpenBrace =>
        event(token, StartOfObjectEvent, new ::(AwaitingFieldNameOrEndOfObject, stack))
      case TokenOpenBracket =>
        event(token, StartOfArrayEvent, new ::(AwaitingEntryOrEndOfArray, stack))
      case TokenIdentifier(text) =>
        event(token, IdentifierEvent(text), stack)
      case TokenNumber(number) =>
        event(token, NumberEvent(number), stack)
      case TokenString(string) =>
        event(token, StringEvent(string), stack)
      case _ =>
        error(token, "datum")
    }

    override def toString = "AwaitingDatum"
  }

  private val AwaitingEntryOrEndOfArray: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenOpenBrace =>
        event(token, StartOfObjectEvent, new ::(AwaitingFieldNameOrEndOfObject, AwaitingCommaOrEndOfArray :: stack))
      case TokenOpenBracket =>
        event(token, StartOfArrayEvent, new ::(AwaitingEntryOrEndOfArray, AwaitingCommaOrEndOfArray :: stack))
      case TokenIdentifier(text) =>
        event(token, IdentifierEvent(text), new ::(AwaitingCommaOrEndOfArray, stack))
      case TokenNumber(number) =>
        event(token, NumberEvent(number), new ::(AwaitingCommaOrEndOfArray, stack))
      case TokenString(string) =>
        event(token, StringEvent(string), new ::(AwaitingCommaOrEndOfArray, stack))
      case TokenCloseBracket =>
        event(token, EndOfArrayEvent, stack)
      case _ =>
        error(token, "datum or end of list")
    }

    override def toString = "AwaitingEntryOrEndOfArray"
  }

  private val AwaitingCommaOrEndOfArray: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenComma =>
        more(new ::(AwaitingDatum, AwaitingCommaOrEndOfArray :: stack))
      case TokenCloseBracket =>
        event(token, EndOfArrayEvent, stack)
      case _ =>
        error(token, "comma or end of list")
    }

    override def toString = "AwaitingCommaOrEndOfArray"
  }

  private val AwaitingFieldNameOrEndOfObject: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenCloseBrace =>
        event(token, EndOfObjectEvent, stack)
      case TokenString(text) =>
        event(token, FieldEvent(text), new ::(AwaitingKVSep, AwaitingCommaOrEndOfObject :: stack))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(text), new ::(AwaitingKVSep, AwaitingCommaOrEndOfObject :: stack))
      case _ =>
        error(token, "field name or end of object")
    }

    override def toString = "AwaitingFieldNameOrEndOfObject"
  }

  private val AwaitingFieldName: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenString(text) =>
        event(token, FieldEvent(text), new ::(AwaitingKVSep, stack))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(text), new ::(AwaitingKVSep, stack))
      case _ =>
        error(token, "field name")
    }

    override def toString = "AwaitingFieldName"
  }

  private val AwaitingKVSep: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenColon =>
        more(new ::(AwaitingDatum, stack))
      case _ =>
        error(token, "colon")
    }

    override def toString = "AwaitingKVSep"
  }

  private val AwaitingCommaOrEndOfObject: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenComma =>
        more(new ::(AwaitingFieldName, AwaitingCommaOrEndOfObject :: stack))
      case TokenCloseBrace =>
        event(token, EndOfObjectEvent, stack)
      case _ =>
        error(token, "comma or end of object")
    }

    override def toString = "AwaitingCommaOrEndOfObject"
  }

  val newParser = new JsonParser(new ::(AwaitingDatum, Nil))
}

sealed abstract class ParseResult
case class ParseError(token: JsonToken, expected: String, row: Int, col: Int) extends ParseResult
sealed abstract class SuccessfulParseResult extends ParseResult
case class More(newState: JsonParser) extends SuccessfulParseResult
case class Event(event: PositionedJsonEvent, newState: JsonParser) extends SuccessfulParseResult
