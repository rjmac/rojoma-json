package com.rojoma.json.v3
package util

import codec._
import io._
import `-impl`.util.AbstractIterator

import java.io.Reader

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
  def fromReader[T : JsonDecode](reader: Reader, alreadyInArray: Boolean = false, buffer: Boolean = true): Iterator[T] = {
    val events =
      if(buffer) new FusedBlockJsonEventIterator(reader)
      else new JsonEventIterator(reader)
    fromEvents(events, alreadyInArray)
  }

  @deprecated(message = "Prefer fromEvents", since = "3.5.0")
  def apply[T : JsonDecode](input: Iterator[JsonEvent], alreadyInArray: Boolean = false): Iterator[T] =
    fromEvents[T](input, alreadyInArray)

  def fromEvents[T : JsonDecode](input: Iterator[JsonEvent], alreadyInArray: Boolean = false): Iterator[T] = {
    val buffer = input.buffered
    var item = 0

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
        item += 1
        JsonDecode.fromJValue[T](JsonReader.fromEvents(buffer)) match {
          case Right(res) => res
          case Left(err) => throw new ElementDecodeException(pos, err.augment(Path.Index(item - 1)))
        }
      }
    }
  }

  class ElementDecodeException(val position: Position, val error: DecodeError) extends RuntimeException((if (position.isValid) position + ": " else "") + error.english)
}
