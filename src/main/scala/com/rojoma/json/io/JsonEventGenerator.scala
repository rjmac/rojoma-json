package com.rojoma.json
package io

import JsonEventGenerator._
import JsonEventGeneratorImpl._

sealed abstract class JsonEventGenerator private[io] (protected val stack: Stack) {
  def apply(token: PositionedJsonToken): Result
  protected def name: String

  def parse(token: PositionedJsonToken): SuccessfulResult = apply(token) match {
    case r: SuccessfulResult => r
    case e: Error => throwError(e)
  }

  def endOfInput(row: Int, col: Int): EndResult = {
    if(atTopLevel) Finished
    else Unfinished(row, col)
  }

  def finish(row: Int, col: Int): SuccessfulEndResult = {
    endOfInput(row, col) match {
      case r: SuccessfulEndResult => r
      case e: EndError => throwError(e)
    }
  }

  def atTopLevel: Boolean = (stack eq null) && this.isInstanceOf[AwaitingDatum]

  override def equals(o: Any): Boolean = o match {
    case that: JsonEventGenerator => locallySame(that) && this.stack == that.stack
    case _ => false
  }

  override def hashCode: Int = localHashCode ^ stack.hashCode

  override def toString = {
    val sb = new StringBuilder("JsonEventGenerator(").append(name)
    var it = stack
    while(it ne null) {
      sb.append(',').append(it.name)
      it = it.stack
    }
    sb.append(')').mkString
  }

  protected def localHashCode: Int = getClass.hashCode
  protected def locallySame(that: JsonEventGenerator): Boolean = this.getClass == that.getClass

  protected def event(token: PositionedJsonToken, ev: JsonEvent, newState: JsonEventGenerator): Result = {
    val pEv = PositionedJsonEvent(ev, token.row, token.column)
    val trueNewState = if(newState eq null) newGenerator else newState
    Event(pEv, trueNewState)
  }

  protected def more(newState: JsonEventGenerator): Result =
    More(newState)

  protected def error(got: PositionedJsonToken, expected: String): Result =
    UnexpectedToken(got.token, expected, got.row, got.column)
}

object JsonEventGenerator {
  sealed trait AnyError

  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result with AnyError

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult with AnyError

  case class More(newState: JsonEventGenerator) extends SuccessfulResult
  case class Event(event: PositionedJsonEvent, newState: JsonEventGenerator) extends SuccessfulResult

  case object Finished extends SuccessfulEndResult

  case class UnexpectedToken(token: JsonToken, expected: String, row: Int, col: Int) extends Error with EndError
  case class Unfinished(row: Int, col: Int) extends EndError

  val newGenerator: JsonEventGenerator = new AwaitingDatum(null)

  def throwError(e: AnyError): Nothing = e match {
    case UnexpectedToken(token, expected, row, col) => throw new JsonUnexpectedToken(token, expected, row, col)
    case Unfinished(row, col) => throw new JsonEOF(row, col)
  }
}

private[io] object JsonEventGeneratorImpl {
  type Stack = JsonEventGenerator

  class AwaitingDatum(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenOpenBrace =>
        event(token, StartOfObjectEvent, new AwaitingFieldNameOrEndOfObject(stack))
      case TokenOpenBracket =>
        event(token, StartOfArrayEvent, new AwaitingEntryOrEndOfArray(stack))
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

  class AwaitingEntryOrEndOfArray(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenOpenBrace =>
        event(token, StartOfObjectEvent, new AwaitingFieldNameOrEndOfObject(new AwaitingCommaOrEndOfArray(stack)))
      case TokenOpenBracket =>
        event(token, StartOfArrayEvent, new AwaitingEntryOrEndOfArray(new AwaitingCommaOrEndOfArray(stack)))
      case TokenIdentifier(text) =>
        event(token, IdentifierEvent(text), new AwaitingCommaOrEndOfArray(stack))
      case TokenNumber(number) =>
        event(token, NumberEvent(number), new AwaitingCommaOrEndOfArray(stack))
      case TokenString(string) =>
        event(token, StringEvent(string), new AwaitingCommaOrEndOfArray(stack))
      case TokenCloseBracket =>
        event(token, EndOfArrayEvent, stack)
      case _ =>
        error(token, "datum or end of list")
    }

    def name = "AwaitingEntryOrEndOfArray"
  }

  class AwaitingCommaOrEndOfArray(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenComma =>
        more(new AwaitingDatum(new AwaitingCommaOrEndOfArray(stack)))
      case TokenCloseBracket =>
        event(token, EndOfArrayEvent, stack)
      case _ =>
        error(token, "comma or end of list")
    }

    def name = "AwaitingCommaOrEndOfArray"
  }

  class AwaitingFieldNameOrEndOfObject(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenCloseBrace =>
        event(token, EndOfObjectEvent, stack)
      case TokenString(text) =>
        event(token, FieldEvent(text), new AwaitingKVSep(new AwaitingCommaOrEndOfObject(stack)))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(text), new AwaitingKVSep(new AwaitingCommaOrEndOfObject(stack)))
      case _ =>
        error(token, "field name or end of object")
    }

    def name = "AwaitingFieldNameOrEndOfObject"
  }

  class AwaitingFieldName(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenString(text) =>
        event(token, FieldEvent(text), new AwaitingKVSep(stack))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(text), new AwaitingKVSep(stack))
      case _ =>
        error(token, "field name")
    }

    def name = "AwaitingFieldName"
  }

  class AwaitingKVSep(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenColon =>
        more(new AwaitingDatum(stack))
      case _ =>
        error(token, "colon")
    }

    def name = "AwaitingKVSep"
  }

  class AwaitingCommaOrEndOfObject(s: Stack) extends JsonEventGenerator(s) {
    def apply(token: PositionedJsonToken) = token.token match {
      case TokenComma =>
        more(new AwaitingFieldName(new AwaitingCommaOrEndOfObject(stack)))
      case TokenCloseBrace =>
        event(token, EndOfObjectEvent, stack)
      case _ =>
        error(token, "comma or end of object")
    }

    def name = "AwaitingCommaOrEndOfObject"
  }
}
