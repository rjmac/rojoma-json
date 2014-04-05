package com.rojoma.json.v2
package io

import com.rojoma.`json-impl`.AbstractIterator
import com.rojoma.`json-impl`.FlatteningIterator
import com.rojoma.`json-impl`.FlatteningIteratorUtils._

case class MalformedEventStreamException(message: String) extends RuntimeException(message)

/** A function which converts an `Iterator[JsonEvent]` into an `Iterator[JsonToken]`.
 *
 * @param input The iterator of events to convert to tokens
 * @return An iterator of tokens that can be used to generate textual output
 * @throws MalformedEventStreamException if the event stream is not well-formed
 */
object EventTokenIterator extends (Iterator[JsonEvent] => Iterator[JsonToken]) {
  private val closeBrace = TokenCloseBrace()(Position.Invalid)
  private val closeBracket = TokenCloseBracket()(Position.Invalid)
  private val colon = TokenColon()(Position.Invalid)
  private val comma = TokenComma()(Position.Invalid)

  def apply(input: Iterator[JsonEvent]): Iterator[JsonToken] = new AbstractIterator[JsonToken] {
    private val buffer = input.buffered
    private var nextObject: Iterator[JsonToken] = Iterator.empty
    def hasNext = nextObject.hasNext || buffer.hasNext
    def next() = {
      if(!nextObject.hasNext) nextObject = tokenizeDatum()
      nextObject.next()
    }

    private def tokenizeDatum(): Iterator[JsonToken] = buffer.next() match {
      case t@StartOfObjectEvent() =>
        Iterator.single(TokenOpenBrace()(t.position)) ** iteratorForObject().flatify ** Iterator.single(closeBrace)
      case t@StartOfArrayEvent() =>
        Iterator.single(TokenOpenBracket()(t.position)) ** iteratorForArray().flatify ** Iterator.single(closeBracket)
      case t@IdentifierEvent(identifier) =>
        Iterator.single(TokenIdentifier(identifier)(t.position))
      case t@NumberEvent(number) =>
        Iterator.single(TokenNumber(number)(t.position))
      case t@StringEvent(string) =>
        Iterator.single(TokenString(string)(t.position))
      case FieldEvent(_) =>
        throw MalformedEventStreamException("Expected datum; received FieldEvent")
      case EndOfObjectEvent() =>
        throw MalformedEventStreamException("Expected datum: recevied EndOfObjectEvent")
      case EndOfArrayEvent() =>
        throw MalformedEventStreamException("Expected datum: recevied EndOfArrayEvent")
    }

    private def iteratorForArray() = new AbstractIterator[Iterator[JsonToken]] { outer =>
      private var done = false

      def hasNext =
        if(done) {
          false
        } else if(buffer.isEmpty) {
          throw MalformedEventStreamException("End of input inside array")
        } else if(buffer.head.isInstanceOf[EndOfArrayEvent]) {
          done = true
          buffer.next()
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
          throw MalformedEventStreamException("End of input inside object")
        } else if(buffer.head.isInstanceOf[EndOfObjectEvent]) {
          done = true
          buffer.next()
          false
        } else {
          true
        }

      def next() = {
        if(!hasNext) Iterator.empty.next()
        buffer.next() match {
          case t@FieldEvent(field) =>
            Iterator(TokenString(field)(t.position), colon) ** tokenizeDatum() ** new AbstractIterator[JsonToken] {
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
            throw MalformedEventStreamException("Expected FieldEvent, received " + other.getClass.getName)
        }
      }
    }
  }
}
