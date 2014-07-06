package com.rojoma.json.v3
package io

import com.rojoma.json.v3.`-impl`.util.FlatteningIterator
import com.rojoma.json.v3.`-impl`.util.FlatteningIteratorUtils._

import ast._

/** A function which converts a `JValue` into an `Iterator[JsonEvent]`.
 *
 * @param value The value to convert
 * @return An iterator of events that can be re-parsed or converted into tokens for output
 */
object JValueEventIterator extends (JValue => Iterator[JsonEvent]) {
  private val SoO = StartOfObjectEvent()(Position.Invalid)
  private val EoO = EndOfObjectEvent()(Position.Invalid)
  private val SoA = StartOfArrayEvent()(Position.Invalid)
  private val EoA = EndOfArrayEvent()(Position.Invalid)
  private val True = IdentifierEvent("true")(Position.Invalid)
  private val False = IdentifierEvent("false")(Position.Invalid)
  private val Null = IdentifierEvent("null")(Position.Invalid)

  def apply(value: JValue): Iterator[JsonEvent] =
    value match {
      case JObject(fields) =>
        Iterator.single(SoO) ** fields.iterator.map { case (k,v) => Iterator.single(FieldEvent(k)(Position.Invalid)) ** apply(v) }.flatify ** Iterator.single(EoO)
      case JArray(elems) =>
        Iterator.single(SoA) ** elems.iterator.map(apply).flatify ++ Iterator.single(EoA)
      case JString(string) =>
        Iterator.single(StringEvent(string)(Position.Invalid))
      case number: JNumber =>
        Iterator.single(NumberEvent(number.toString)(Position.Invalid))
      case JBoolean(true) =>
        Iterator.single(True)
      case JBoolean(false) =>
        Iterator.single(False)
      case JNull =>
        Iterator.single(Null)
    }
}
