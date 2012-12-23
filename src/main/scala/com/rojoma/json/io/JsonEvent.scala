package com.rojoma.json
package io

sealed abstract class JsonEvent {
  var position = Position.Invalid
}

case class StartOfObjectEvent() extends JsonEvent
case class EndOfObjectEvent() extends JsonEvent
case class StartOfArrayEvent() extends JsonEvent
case class EndOfArrayEvent() extends JsonEvent
case class FieldEvent(name: String) extends JsonEvent
case class IdentifierEvent(text: String) extends JsonEvent
case class NumberEvent(number: BigDecimal) extends JsonEvent
case class StringEvent(string: String) extends JsonEvent
