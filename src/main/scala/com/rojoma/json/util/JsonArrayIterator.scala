package com.rojoma.json
package util

import codec.JsonCodec
import io._

/** Helper for reading lazily reading objects out of a source of
 * `JsonEvent`s representing a JSON array.  Calling `hasNext` can throw
 * any `JsonLexException`.  Calling `next()` can throw any JSON lex or parse
 * exception, or `ElementDecodeException` if the data in the array cannot be
 * decoded as a `T` at that point.
 *
 * @param input A source of JSON events
 * @param alreadyInArray A flag to indicate the start-of-array event has already been consumed (default false)
 * @return An iterator of `T`s
 * @throws JsonLexException if `alreadyInArray` is false and a lexing exception or EOF occurs.
 * @throws JsonBadParse if `alreadyInArray` is false and the first event is not a `StartOfArrayEvent`
 */
object JsonArrayIterator {
  def apply[T : JsonCodec](input: Iterator[PositionedJsonEvent], alreadyInArray: Boolean = false): Iterator[T] = {
    val buffer = input.buffered

    if(!alreadyInArray) {
      val next = try {
        buffer.next()
      } catch {
        case NoSuchTokenException(row, col) => throw JsonEOF(row, col)
        case _: NoSuchElementException => throw JsonEOF(-1, -1)
      }
      if(next.event != StartOfArrayEvent) throw JsonBadParse(next.event, next.row, next.column)
    }

    new Iterator[T] {
      private var done = false

      def hasNext: Boolean = {
        if(done) return false
        val head = try {
          buffer.head
        } catch {
          case NoSuchTokenException(row, col) => throw JsonEOF(row, col)
          case _: NoSuchElementException => throw JsonEOF(-1, -1)
        }
        if(head.event == EndOfArrayEvent) {
          buffer.next()
          done = true
          return false
        }
        true
      }

      def next(): T = {
        if(!hasNext) Iterator.empty.next()
        val row = buffer.head.row
        val col = buffer.head.column
        JsonCodec[T].decode(JsonReader.fromEvents(buffer)).getOrElse {
          throw ElementDecodeException(row, col)
        }
      }
    }
  }

  case class ElementDecodeException(row: Int, col: Int) extends RuntimeException("Unable to decode array element as specified type")
}
