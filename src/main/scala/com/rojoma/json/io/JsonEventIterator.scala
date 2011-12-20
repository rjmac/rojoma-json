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

class JsonEventIterator(underlying: Iterator[PositionedJsonToken]) extends BufferedIterator[PositionedJsonEvent] {
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

  def head = {
    if(available == null && !hasNext) throw new NoSuchElementException("Empty iterator")
    available
  }

  def next() = {
    val result = head
    available = null
    result
  }

  /**
   * Finish reading the "current" object or list, where "current" is
   * defined as "until the end of the most recent compound object
   * started by `next()` or `head`, depending on whether `fromHead`
   * is true".  If `fromHead` is false and next() has not been called,
   * this does nothing.  If this iterator is empty, a `NoSuchElementException`
   * will be thrown.
   */
  def skipRestOfCompound(fromHead: Boolean = false): JsonEventIterator = {
    if(fromHead) {
      next()
    } else if(stack.size == 1 && stack(0) == AwaitingDatum) {
      return this
    }
    var count = 0
    do {
      val ev = next().event
      ev match {
        case StartOfObjectEvent | StartOfArrayEvent => count += 1
        case EndOfObjectEvent | EndOfArrayEvent => count -= 1
        case _ => /* nothing */
      }
    } while(hasNext && count >= 0)
    this
  }

  /** Skips the next datum that would be returned entirely.  If the next event
   * is the start of a list or object, `skipRestOfCompound()` is called to
   * pass over it. If it's a field event, the field and its associated value
   * are skipped. If it's the end of a list or object, no position change is
   * made and the next call to `head` or `next()` will still return the end
   * event.  Otherwise, it's an atom and is consumed.
   *
   * If the iterator is empty at the start of this call, `NoSuchElementException`
   * is raised.
   */
  def skipNextDatum(): JsonEventIterator = {
    head.event match {
      case StartOfObjectEvent | StartOfArrayEvent => skipRestOfCompound(fromHead = true)
      case FieldEvent(_) => next(); skipNextDatum()
      case EndOfObjectEvent | EndOfArrayEvent => this
      case _ => next(); this
    }
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
