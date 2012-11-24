package com.rojoma.json
package io

import JsonEventGenerator._
import JsonEventGeneratorImpl._

sealed abstract class JsonEventGenerator private[io] (val fieldCache: FieldCache, protected val stack: Stack) {
  def apply(token: JsonToken): Result
  protected def name: String

  def parse(token: JsonToken): SuccessfulResult = apply(token) match {
    case r: SuccessfulResult => r
    case e: Error => throwError(e)
  }

  def endOfInput(position: Position): EndResult = {
    if(atTopLevel) Finished
    else Unfinished(position)
  }

  def endOfInput(row: Int, column: Int): EndResult = endOfInput(Position(row, column))

  def finish(position: Position): SuccessfulEndResult = {
    endOfInput(position) match {
      case r: SuccessfulEndResult => r
      case e: EndError => throwError(e)
    }
  }

  def finish(row: Int, column: Int): SuccessfulEndResult = finish(Position(row, column))

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

  protected def event(token: JsonToken, ev: JsonEvent, newState: JsonEventGenerator): Result = {
    ev.row = token.row
    ev.column = token.column
    val trueNewState = if(newState eq null) newGenerator(fieldCache) else newState
    Event(ev, trueNewState)
  }

  protected def more(newState: JsonEventGenerator): Result =
    More(newState)

  protected def error(got: JsonToken, expected: String): Result =
    UnexpectedToken(got, expected)
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
  case class Event(event: JsonEvent, newState: JsonEventGenerator) extends SuccessfulResult

  case object Finished extends SuccessfulEndResult

  case class UnexpectedToken(token: JsonToken, expected: String) extends Error with EndError
  case class Unfinished(position: Position) extends EndError

  val newGenerator: JsonEventGenerator = newGenerator(IdentityFieldCache)
  def newGenerator(fieldCache: FieldCache): JsonEventGenerator = new AwaitingDatum(fieldCache, null)

  def throwError(e: AnyError): Nothing = e match {
    case UnexpectedToken(token, expected) => throw new JsonUnexpectedToken(token, expected)
    case Unfinished(pos) => throw new JsonParserEOF(pos)
  }
}

private[io] object JsonEventGeneratorImpl {
  type Stack = JsonEventGenerator

  class AwaitingDatum(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenOpenBrace() =>
        event(token, StartOfObjectEvent(), new AwaitingFieldNameOrEndOfObject(fieldCache, stack))
      case TokenOpenBracket() =>
        event(token, StartOfArrayEvent(), new AwaitingEntryOrEndOfArray(fieldCache, stack))
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

  class AwaitingEntryOrEndOfArray(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenOpenBrace() =>
        event(token, StartOfObjectEvent(), new AwaitingFieldNameOrEndOfObject(fieldCache, new AwaitingCommaOrEndOfArray(fieldCache, stack)))
      case TokenOpenBracket() =>
        event(token, StartOfArrayEvent(), new AwaitingEntryOrEndOfArray(fieldCache, new AwaitingCommaOrEndOfArray(fieldCache, stack)))
      case TokenIdentifier(text) =>
        event(token, IdentifierEvent(text), new AwaitingCommaOrEndOfArray(fieldCache, stack))
      case TokenNumber(number) =>
        event(token, NumberEvent(number), new AwaitingCommaOrEndOfArray(fieldCache, stack))
      case TokenString(string) =>
        event(token, StringEvent(string), new AwaitingCommaOrEndOfArray(fieldCache, stack))
      case TokenCloseBracket() =>
        event(token, EndOfArrayEvent(), stack)
      case _ =>
        error(token, "datum or end of list")
    }

    def name = "AwaitingEntryOrEndOfArray"
  }

  class AwaitingCommaOrEndOfArray(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenComma() =>
        more(new AwaitingDatum(fieldCache, new AwaitingCommaOrEndOfArray(fieldCache, stack)))
      case TokenCloseBracket() =>
        event(token, EndOfArrayEvent(), stack)
      case _ =>
        error(token, "comma or end of list")
    }

    def name = "AwaitingCommaOrEndOfArray"
  }

  class AwaitingFieldNameOrEndOfObject(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenCloseBrace() =>
        event(token, EndOfObjectEvent(), stack)
      case TokenString(text) =>
        event(token, FieldEvent(fieldCache(text)), new AwaitingKVSep(fieldCache, new AwaitingCommaOrEndOfObject(fieldCache, stack)))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(fieldCache(text)), new AwaitingKVSep(fieldCache, new AwaitingCommaOrEndOfObject(fieldCache, stack)))
      case _ =>
        error(token, "field name or end of object")
    }

    def name = "AwaitingFieldNameOrEndOfObject"
  }

  class AwaitingFieldName(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenString(text) =>
        event(token, FieldEvent(fieldCache(text)), new AwaitingKVSep(fieldCache, stack))
      case TokenIdentifier(text) =>
        event(token, FieldEvent(fieldCache(text)), new AwaitingKVSep(fieldCache, stack))
      case _ =>
        error(token, "field name")
    }

    def name = "AwaitingFieldName"
  }

  class AwaitingKVSep(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenColon() =>
        more(new AwaitingDatum(fieldCache, stack))
      case _ =>
        error(token, "colon")
    }

    def name = "AwaitingKVSep"
  }

  class AwaitingCommaOrEndOfObject(fc: FieldCache, s: Stack) extends JsonEventGenerator(fc, s) {
    def apply(token: JsonToken) = token match {
      case TokenComma() =>
        more(new AwaitingFieldName(fieldCache, new AwaitingCommaOrEndOfObject(fieldCache, stack)))
      case TokenCloseBrace() =>
        event(token, EndOfObjectEvent(), stack)
      case _ =>
        error(token, "comma or end of object")
    }

    def name = "AwaitingCommaOrEndOfObject"
  }
}
