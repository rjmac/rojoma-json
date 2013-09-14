package com.rojoma.json
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
        Iterator.single(StartOfObjectEvent()) ** fields.iterator.map { case (k,v) => Iterator.single(FieldEvent(k)) ** apply(v) }.flatify ** Iterator.single(EndOfObjectEvent())
      case JArray(elems) =>
        Iterator.single(StartOfArrayEvent()) ** elems.iterator.map(apply).flatify ++ Iterator.single(EndOfArrayEvent())
      case JString(string) =>
        Iterator.single(StringEvent(string))
      case JNumber(number) =>
        Iterator.single(NumberEvent(number))
      case JBoolean(true) =>
        Iterator.single(IdentifierEvent("true"))
      case JBoolean(false) =>
        Iterator.single(IdentifierEvent("false"))
      case JNull =>
        Iterator.single(IdentifierEvent("null"))
    }
}
