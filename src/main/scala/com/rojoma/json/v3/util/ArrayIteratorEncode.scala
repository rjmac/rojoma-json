package com.rojoma.json.v3
package util

import io._
import codec.JsonEncode

/** Converting iterators-of-jsonables to iterators that represent
  * `JArray`s, without holding onto the contents of the iterator.
  */
object ArrayIteratorEncode {
  /** Takes an iterator of jsonable objects and returns an iterator
    * equivalent to `JValueEventIterator(JArray(it.toSeq))`
    */
  def toEvents[T : JsonEncode](it : Iterator[T]): Iterator[JsonEvent] =
    Iterator.single(StartOfArrayEvent()(Position.Invalid)) ++
      it.flatMap { item => JValueEventIterator(JsonEncode.toJValue(item)) } ++
      Iterator.single(EndOfArrayEvent()(Position.Invalid))

  /** Takes an iterator of jsonable objects and returns an iterator
    * equivalent to `EventTokenIterator(JValueEventIterator(JArray(it.toSeq)))`
    */
  def toTokens[T : JsonEncode](it : Iterator[T]): Iterator[JsonToken] =
    EventTokenIterator(toEvents(it))

  /** Takes an iterator of jsonable objects and returns an iterator of
    * text fragments of `JArray(it.toSeq).toString`
    */
  def toText[T : JsonEncode](it : Iterator[T]): Iterator[String] =
    toTokens(it).map(_.asFragment)
}
