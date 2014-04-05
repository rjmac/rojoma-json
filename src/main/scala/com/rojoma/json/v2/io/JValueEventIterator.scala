package com.rojoma.json.v2
package io

import com.rojoma.`json-impl`.FlatteningIterator
import com.rojoma.`json-impl`.FlatteningIteratorUtils._

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
        Iterator.single(sooEvent) ** fields.iterator.map { case (k,v) => Iterator.single(FieldEvent(k)(Position.Invalid)) ** apply(v) }.flatify ** Iterator.single(eooEvent)
      case JArray(elems) =>
        Iterator.single(soaEvent) ** elems.iterator.map(apply).flatify ++ Iterator.single(eoaEvent)
      case JString(string) =>
        Iterator.single(StringEvent(string)(Position.Invalid))
      case number: JNumber =>
        Iterator.single(NumberEvent(number.toString)(Position.Invalid))
      case JBoolean(true) =>
        Iterator.single(trueEvent)
      case JBoolean(false) =>
        Iterator.single(falseEvent)
      case JNull =>
        Iterator.single(nullEvent)
    }

  private val sooEvent = StartOfObjectEvent()(Position.Invalid)
  private val eooEvent = EndOfObjectEvent()(Position.Invalid)
  private val soaEvent = StartOfArrayEvent()(Position.Invalid)
  private val eoaEvent = EndOfArrayEvent()(Position.Invalid)
  private val trueEvent = IdentifierEvent("true")(Position.Invalid)
  private val falseEvent = IdentifierEvent("false")(Position.Invalid)
  private val nullEvent = IdentifierEvent("null")(Position.Invalid)
}
