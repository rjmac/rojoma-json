package com.rojoma.json
package io

import com.rojoma.`json-impl`.BoundedIterator

import ast._

/** A function which converts a `JValue` into an `Iterator[JsonEvent]`.
 *
 * @param value The value to convert
 * @return An iterator of events that can be re-parsed or converted into tokens for output
 */
object JValueEventIterator extends (JValue => Iterator[JsonEvent]) {
  def apply(value: JValue): Iterator[JsonEvent] =
    value match {
      case JObject(fields) =>
        new BoundedIterator(StartOfObjectEvent, fields.iterator.flatMap { case (k,v) => Iterator.single(FieldEvent(k)) ++ apply(v) }, EndOfObjectEvent)
      case JArray(elems) =>
        new BoundedIterator(StartOfArrayEvent, elems.iterator.flatMap(apply), EndOfArrayEvent)
      case JString(string) =>
        Iterator.single(StringEvent(string))
      case JNumber(number) =>
        Iterator.single(NumberEvent(number))
      case JBoolean(true) =>
        Iterator.single(trueEvent)
      case JBoolean(false) =>
        Iterator.single(falseEvent)
      case JNull =>
        Iterator.single(nullEvent)
    }

  private val trueEvent = IdentifierEvent("true")
  private val falseEvent = IdentifierEvent("false")
  private val nullEvent = IdentifierEvent("null")
}
