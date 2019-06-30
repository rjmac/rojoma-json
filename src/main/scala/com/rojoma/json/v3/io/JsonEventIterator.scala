package com.rojoma.json.v3
package io

import java.io.Reader

import `-impl`.util.AbstractBufferedIterator

/** Turns a raw token-stream into an event stream, checking for JSON
 * well-formedness.
 *
 * A `JsonEventIterator` checks a token stream for syntactic correctness
 * and produces events that reflect the syntax of JSON.  It guarantees
 * to demand no more input from the input iterator than is absolutely
 * required.
 *
 * As an extension, this class allows unquoted identifiers to be used
 * as object keys.
 *
 * @see [[com.rojoma.json.v3.io.FusedBlockJsonEventIterator]]
 * @see [[com.rojoma.json.v3.io.JsonEventGenerator]]
 * @see [[com.rojoma.json.v3.io.JsonEvent]]
 */
class JsonEventIterator(input: Iterator[JsonToken], fieldCache: FieldCache) extends AbstractBufferedIterator[JsonEvent] {
  def this(reader: Reader, fieldCache: FieldCache) = this(new JsonTokenIterator(reader), fieldCache)
  def this(text: String, fieldCache: FieldCache) = this(new BlockJsonTokenIterator(text), fieldCache)

  def this(input: Iterator[JsonToken]) = this(input, IdentityFieldCache)
  def this(reader: Reader) = this(reader, IdentityFieldCache)
  def this(text: String) = this(text, IdentityFieldCache)

  private var parser = JsonEventGenerator.newGenerator(fieldCache)
  private val underlying = input.buffered
  private var available: JsonEvent = null
  private var atTop = true // this reflects the state *before* the feed that resulted in "available" being set.

  override def toString =
    if(available ne null) "non-empty iterator"
    else input.toString

  def hasNext: Boolean = {
    if(available ne null) {
      true
    } else {
      atTop = parser.atTopLevel
      while((available eq null) && underlying.hasNext) {
        val token = underlying.next()
        parser.parse(token) match {
          case JsonEventGenerator.Event(ev, newParser) =>
            available = ev
            parser = newParser
          case JsonEventGenerator.More(newParser) =>
            parser = newParser
        }
      }
      available != null
    }
  }

  def head = {
    if(available == null && !hasNext) {
      underlying.next() // force NoSuchElementException
    }
    available
  }

  def next() = {
    val result = head
    available = null
    result
  }

  /**
   * Finish reading the "current" object or array, where "current" is
   * defined as "the most recent compound object started by `next()`.
   * If a top-level object has not been started, this does nothing.
   *
   * @return This iterator
   * @throws JsonEOF If the end-of-input occurs before finishing
   *   this object.
   */
  def skipRestOfCompound(): this.type = {
    hasNext // hasNext to make sure atTop is in an accurate state
    if(!atTop) {
      try {
        var count = 0
        do {
          val ev = next()
          ev match {
            case StartOfObjectEvent() | StartOfArrayEvent() => count += 1
            case EndOfObjectEvent() | EndOfArrayEvent() => count -= 1
            case _ => /* nothing */
          }
        } while(count >= 0)
      } catch {
        case e: NoSuchTokenException => throw new JsonParserEOF(e.position)
        case _: NoSuchElementException => throw new JsonParserEOF(Position(-1, -1))
      }
    }
    this
  }

  @inline
  final def dropRestOfCompound() = skipRestOfCompound()

  /** Skips the next datum that would be returned entirely.  If the next event
   * is the start of a array or object, `skipRestOfCompound()` is called to
   * pass over it. If it's a field event, the field and its associated value
   * are skipped. If it's the end of a array or object, no position change is
   * made and the next call to `head` or `next()` will still return the end
   * event.  Otherwise, it's an atom and is consumed.
   *
   * @return This iterator
   * @throws NoSuchElementException if this iterator is empty at the start of the call
   * @throws JsonEOF if the token iterator runs out before the end of the datum
   */
  def skipNextDatum(): this.type = head match {
    case StartOfObjectEvent() | StartOfArrayEvent() =>
      next()
      skipRestOfCompound()
    case FieldEvent(_) =>
      next()
      skipNextDatum()
    case EndOfObjectEvent() | EndOfArrayEvent() =>
      this
    case _ =>
      next()
      this
  }

  @inline
  final def dropNextDatum() = skipNextDatum()
}

object JsonEventIterator {
  private type StateStack = scala.collection.mutable.Stack[State]

  private abstract class State {
    protected def error(got: JsonToken, expected: String): Nothing =
      throw new JsonUnexpectedToken(got, expected)

    def handle(token: JsonToken, stack: StateStack): JsonEvent
  }

  private val AwaitingDatum: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenOpenBrace() =>
        stack.push(AwaitingFieldNameOrEndOfObject)
        StartOfObjectEvent()(token.position)
      case TokenOpenBracket() =>
        stack.push(AwaitingEntryOrEndOfArray)
        StartOfArrayEvent()(token.position)
      case TokenIdentifier(text) =>
        IdentifierEvent(text)(token.position)
      case TokenNumber(number) =>
        NumberEvent(number)(token.position)
      case TokenString(string) =>
        StringEvent(string)(token.position)
      case _ =>
        error(token, "datum")
    }
  }

  private val AwaitingEntryOrEndOfArray: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenOpenBrace() =>
        stack.push(AwaitingCommaOrEndOfArray)
        stack.push(AwaitingFieldNameOrEndOfObject)
        StartOfObjectEvent()(token.position)
      case TokenOpenBracket() =>
        stack.push(AwaitingCommaOrEndOfArray)
        stack.push(AwaitingEntryOrEndOfArray)
        StartOfArrayEvent()(token.position)
      case TokenIdentifier(text) =>
        stack.push(AwaitingCommaOrEndOfArray)
        IdentifierEvent(text)(token.position)
      case TokenNumber(number) =>
        stack.push(AwaitingCommaOrEndOfArray)
        NumberEvent(number)(token.position)
      case TokenString(string) =>
        stack.push(AwaitingCommaOrEndOfArray)
        StringEvent(string)(token.position)
      case TokenCloseBracket() =>
        EndOfArrayEvent()(token.position)
      case _ =>
        error(token, "datum or end of array")
    }
  }

  private val AwaitingCommaOrEndOfArray: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenComma() =>
        stack.push(AwaitingCommaOrEndOfArray)
        stack.push(AwaitingDatum)
        null
      case TokenCloseBracket() =>
        EndOfArrayEvent()(token.position)
      case _ =>
        error(token, "comma or end of array")
    }
  }

  private val AwaitingFieldNameOrEndOfObject: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenCloseBrace() =>
        EndOfObjectEvent()(token.position)
      case TokenString(text) =>
        stack.push(AwaitingCommaOrEndOfObject)
        stack.push(AwaitingKVSep)
        FieldEvent(text)(token.position)
      case TokenIdentifier(text) =>
        stack.push(AwaitingCommaOrEndOfObject)
        stack.push(AwaitingKVSep)
        FieldEvent(text)(token.position)
      case _ =>
        error(token, "field name or end of object")
    }
  }

  private val AwaitingFieldName: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenString(text) =>
        stack.push(AwaitingKVSep)
        FieldEvent(text)(token.position)
      case TokenIdentifier(text) =>
        stack.push(AwaitingKVSep)
        FieldEvent(text)(token.position)
      case _ =>
        error(token, "field name")
    }
  }

  private val AwaitingKVSep: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenColon() =>
        stack.push(AwaitingDatum)
        null
      case _ =>
        error(token, "colon")
    }
  }

  private val AwaitingCommaOrEndOfObject: State = new State {
    def handle(token: JsonToken, stack: StateStack) = token match {
      case TokenComma() =>
        stack.push(AwaitingCommaOrEndOfObject)
        stack.push(AwaitingFieldName)
        null
      case TokenCloseBrace() =>
        EndOfObjectEvent()(token.position)
      case _ =>
        error(token, "comma or end of object")
    }
  }
}
