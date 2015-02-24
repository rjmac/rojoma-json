package com.rojoma.json.v3
package util

import io._
import codec.JsonEncode

/** A function which takes an iterator of jsonable objects and returns
  * an iterator of chunks of the textual representation of the JSON
  * array representing that same object stream.
  */
object ArrayIteratorEncode {
  def apply[T : JsonEncode](it : Iterator[T]): Iterator[String] = {
    val events =
      Iterator.single(StartOfArrayEvent()(Position.Invalid)) ++
        it.flatMap { item => JValueEventIterator(JsonEncode.toJValue(item)) } ++
        Iterator.single(EndOfArrayEvent()(Position.Invalid))

    val tokens = EventTokenIterator(events)

    tokens.map(_.asFragment)
  }
}
