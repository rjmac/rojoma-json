package com.rojoma.json
package io

import com.rojoma.`json-impl`.BoundedIterator
import com.rojoma.`json-impl`.FlatteningIterator

case class MalformedEventStreamException(message: String) extends RuntimeException(message)

/** A function which converts an `Iterator[JsonEvent]` into an `Iterator[JsonToken]`.
 *
 * @param input The iterator of events to convert to tokens
 * @return An iterator of tokens that can be used to generate textual output
 * @throws MalformedEventStreamException if the event stream is not well-formed
 */
object EventTokenIterator extends (Iterator[JsonEvent] => Iterator[JsonToken]) {
  def apply(input: Iterator[JsonEvent]): Iterator[JsonToken] = new Iterator[JsonToken] {
    private val buffer = input.buffered
    private var nextObject: Iterator[JsonToken] = Iterator.empty
    def hasNext = nextObject.hasNext || buffer.hasNext
    def next() = {
      if(!nextObject.hasNext) nextObject = tokenizeDatum()
      nextObject.next()
    }

    private def tokenizeDatum(): Iterator[JsonToken] = buffer.next() match {
      case StartOfObjectEvent() =>
        new BoundedIterator(TokenOpenBrace(), new FlatteningIterator(iteratorForObject()), TokenCloseBrace())
      case StartOfArrayEvent() =>
        new BoundedIterator(TokenOpenBracket(), new FlatteningIterator(iteratorForArray()), TokenCloseBracket())
      case IdentifierEvent(identifier) =>
        Iterator.single(TokenIdentifier(identifier))
      case NumberEvent(number) =>
        Iterator.single(TokenNumber(number))
      case StringEvent(string) =>
        Iterator.single(TokenString(string))
      case FieldEvent(_) =>
        throw MalformedEventStreamException("Expected datum; received FieldEvent")
      case EndOfObjectEvent() =>
        throw MalformedEventStreamException("Expected datum: recevied EndOfObjectEvent")
      case EndOfArrayEvent() =>
        throw MalformedEventStreamException("Expected datum: recevied EndOfArrayEvent")
    }

    private def iteratorForArray() = new Iterator[Iterator[JsonToken]] { outer =>
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
        tokenizeDatum() ++ new Iterator[JsonToken] {
          private var emittedComma = false
          def hasNext =
            if(emittedComma) false
            else outer.hasNext
          def next() = {
            if(!hasNext) Iterator.empty.next()
            emittedComma = true
            TokenComma()
          }
        }
      }
    }

    private def iteratorForObject() = new Iterator[Iterator[JsonToken]] { outer =>
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
          case FieldEvent(field) =>
            Iterator(TokenString(field), TokenColon()) ++ tokenizeDatum() ++ new Iterator[JsonToken] {
              private var emittedComma = false
              def hasNext =
                if(emittedComma) false
                else outer.hasNext
              def next() = {
                if(!hasNext) Iterator.empty.next()
                emittedComma = true
                TokenComma()
              }
            }
          case other =>
            throw MalformedEventStreamException("Expected FieldEvent, received " + other.getClass.getName)
        }
      }
    }
  }
}
