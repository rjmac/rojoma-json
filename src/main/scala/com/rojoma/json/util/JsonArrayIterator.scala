package com.rojoma.json
package util

import codec.JsonCodec
import io._

import com.rojoma.`json-impl`.AbstractIterator

/** Helper for reading lazily reading objects out of a source of
 * `JsonEvent`s representing a JSON array.  Calling `hasNext` can throw
 * any `JsonLexException`.  Calling `next()` can throw any JSON lex or parse
 * exception, or `ElementDecodeException` if the data in the array cannot be
 * decoded as a `T` at that point.  In the latter case, the iterator is still
 * valid and positioned as if the decode had succeeded so it can continue to
 * be used.
 *
 * @param input A source of JSON events
 * @param alreadyInArray A flag to indicate the start-of-array event has already been consumed (default false)
 * @return An iterator of `T`s
 * @throws JsonLexException if `alreadyInArray` is false and a lexing exception or EOF occurs.
 * @throws JsonBadParse if `alreadyInArray` is false and the first event is not a `StartOfArrayEvent`
 */
object JsonArrayIterator {
  def apply[T : JsonCodec](input: Iterator[JsonEvent], alreadyInArray: Boolean = false): Iterator[T] = {
    val buffer = input.buffered

    if(!alreadyInArray) {
      val next = try {
        buffer.next()
      } catch {
        case nst: NoSuchTokenException => throw new JsonParserEOF(nst.position)
        case _: NoSuchElementException => throw new JsonParserEOF(Position.Invalid)
      }
      if(!next.isInstanceOf[StartOfArrayEvent]) throw new JsonBadParse(next)
    }

    new AbstractIterator[T] {
      private var done = false

      def hasNext: Boolean = {
        if(done) return false
        val head = try {
          buffer.head
        } catch {
          case nst: NoSuchTokenException => throw new JsonParserEOF(nst.position)
          case _: NoSuchElementException => throw new JsonParserEOF(Position.Invalid)
        }
        if(head.isInstanceOf[EndOfArrayEvent]) {
          buffer.next()
          done = true
          return false
        }
        true
      }

      def next(): T = {
        if(!hasNext) Iterator.empty.next()
        val pos = buffer.head.position
        JsonCodec[T].decode(JsonReader.fromEvents(buffer)).getOrElse {
          throw new ElementDecodeException(pos)
        }
      }
    }
  }

  class ElementDecodeException(val position: Position) extends RuntimeException("Unable to decode array element as specified type" + (if (position.isValid) " at " + position else ""))
}
