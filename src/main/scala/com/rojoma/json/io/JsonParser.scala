package com.rojoma.json
package io

import JsonParser._

class JsonParser private (val stack: ::[State]) /* extends AnyVal in 2.10? */ {
  // That type for "stack" makes the state-transition code down below a little awkward
  // since "x :: xs" infers to List, not ::.  So in order to get the types done right,
  // it has to be written as "new ::(x, xs)" which has exactly the same meaning but
  // the right type.
  def apply(token: PositionedJsonToken): Result =
    stack.head.handle(token, stack.tail)

  def parse(token: PositionedJsonToken): SuccessfulResult = apply(token) match {
    case r: SuccessfulResult => r
    case Error(token, expected, row, col) => throw new JsonUnexpectedToken(token, expected, row, col)
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
  sealed abstract class Result
  case class Error(token: JsonToken, expected: String, row: Int, col: Int) extends Result
  sealed abstract class SuccessfulResult extends Result
  case class More(newState: JsonParser) extends SuccessfulResult
  case class Event(event: PositionedJsonEvent, newState: JsonParser) extends SuccessfulResult

  sealed abstract class State {
    protected def event(token: PositionedJsonToken, ev: JsonEvent, stack: List[State]): Result = {
      val pEv = PositionedJsonEvent(ev, token.row, token.column)
      stack match {
        case cons@(_ :: _) => Event(pEv, new JsonParser(cons))
        case _ => Event(pEv, newParser)
      }
    }

    protected def more(stack: ::[State]): Result =
      More(new JsonParser(stack))

    protected def error(got: PositionedJsonToken, expected: String): Result =
      Error(got.token, expected, got.row, got.column)

    protected def name: String

    override def toString = name

    def handle(token: PositionedJsonToken, stack: List[State]): Result
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

    def name = "AwaitingDatum"
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

    def name = "AwaitingEntryOrEndOfArray"
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

    def name = "AwaitingCommaOrEndOfArray"
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

    def name = "AwaitingFieldNameOrEndOfObject"
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

    def name = "AwaitingFieldName"
  }

  private val AwaitingKVSep: State = new State {
    def handle(token: PositionedJsonToken, stack: List[State]) = token.token match {
      case TokenColon =>
        more(new ::(AwaitingDatum, stack))
      case _ =>
        error(token, "colon")
    }

    def name = "AwaitingKVSep"
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

    def name = "AwaitingCommaOrEndOfObject"
  }

  val newParser = new JsonParser(new ::(AwaitingDatum, Nil))
}
