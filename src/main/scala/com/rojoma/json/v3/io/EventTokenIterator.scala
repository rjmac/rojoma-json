package com.rojoma.json.v3
package io

import com.rojoma.json.v3.`-impl`.util.AbstractIterator
import com.rojoma.json.v3.`-impl`.util.FlatteningIterator
import com.rojoma.json.v3.`-impl`.util.FlatteningIteratorUtils._

sealed abstract class MalformedEventStreamException(message: String) extends RuntimeException(message)
object MalformedEventStreamException {
  case class UnexpectedEvent(ev: JsonEvent) extends MalformedEventStreamException("Unexpected event: " + ev)
  case class UnexpectedEndOfInput() extends MalformedEventStreamException("Unexpected end of input")
}

/** A function which converts an `Iterator[JsonEvent]` into an `Iterator[JsonToken]`.
 *
 * @param input The iterator of events to convert to tokens
 * @return An iterator of tokens that can be used to generate textual output
 * @throws MalformedEventStreamException if the event stream is not well-formed
 */
object EventTokenIterator extends (Iterator[JsonEvent] => Iterator[JsonToken]) {
  import MalformedEventStreamException._

  def apply(input: Iterator[JsonEvent]): Iterator[JsonToken] = new AbstractIterator[JsonToken] {
    private val buffer = input.buffered
    private var nextObject: Iterator[JsonToken] = Iterator.empty
    def hasNext = nextObject.hasNext || buffer.hasNext
    def next() = {
      if(!nextObject.hasNext) nextObject = tokenizeDatum()
      nextObject.next()
    }

    private def tokenizeDatum(): Iterator[JsonToken] = buffer.next() match {
      case ev@StartOfObjectEvent() =>
        Iterator.single(TokenOpenBrace()(ev.position)) ** iteratorForObject().flatify ** Iterator.single(TokenCloseBrace()(buffer.next().position))
      case ev@StartOfArrayEvent() =>
        Iterator.single(TokenOpenBracket()(ev.position)) ** iteratorForArray().flatify ** Iterator.single(TokenCloseBracket()(buffer.next().position))
      case ev@IdentifierEvent(identifier) =>
        Iterator.single(TokenIdentifier(identifier)(ev.position))
      case ev@NumberEvent(number) =>
        Iterator.single(TokenNumber(number)(ev.position))
      case ev@StringEvent(string) =>
        Iterator.single(TokenString(string)(ev.position))
      case ev@FieldEvent(_) =>
        throw UnexpectedEvent(ev)
      case ev@EndOfObjectEvent() =>
        throw UnexpectedEvent(ev)
      case ev@EndOfArrayEvent() =>
        throw UnexpectedEvent(ev)
    }

    private def iteratorForArray() = new AbstractIterator[Iterator[JsonToken]] { outer =>
      private var done = false

      def hasNext =
        if(done) {
          false
        } else if(buffer.isEmpty) {
          throw UnexpectedEndOfInput()
        } else if(buffer.head.isInstanceOf[EndOfArrayEvent]) {
          done = true
          // don't consume the end
          false
        } else {
          true
        }

      def next() = {
        if(!hasNext) Iterator.empty.next()
        tokenizeDatum() ** new AbstractIterator[JsonToken] {
          private var emittedComma = false
          def hasNext =
            if(emittedComma) false
            else outer.hasNext
          def next() = {
            if(!hasNext) Iterator.empty.next()
            emittedComma = true
            comma
          }
        }
      }
    }

    private def iteratorForObject() = new AbstractIterator[Iterator[JsonToken]] { outer =>
      private var done = false

      def hasNext =
        if(done) {
          false
        } else if(buffer.isEmpty) {
          throw UnexpectedEndOfInput()
        } else if(buffer.head.isInstanceOf[EndOfObjectEvent]) {
          done = true
          // don't consume the end buffer.next()
          false
        } else {
          true
        }

      def next() = {
        if(!hasNext) Iterator.empty.next()
        buffer.next() match {
          case ev@FieldEvent(field) =>
            Iterator(TokenString(field)(ev.position), colon) ** tokenizeDatum() ** new AbstractIterator[JsonToken] {
              private var emittedComma = false
              def hasNext =
                if(emittedComma) false
                else outer.hasNext
              def next() = {
                if(!hasNext) Iterator.empty.next()
                emittedComma = true
                comma
              }
            }
          case other =>
            throw UnexpectedEvent(other)
        }
      }
    }
  }

  private val colon = TokenColon()(Position.Invalid)
  private val comma = TokenComma()(Position.Invalid)
}
