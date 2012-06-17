package com.rojoma.json
package io

sealed abstract class JsonEvent {
  // Since we don't actually have value classes yet, we'll inline this
  // thing manually so they're only actually created if an exception
  // is thrown.  Once we have true value classes, "row" and "column"
  // will go away and these four lines will become just
  //   "var position = Position.Invalid"
  // See also JsonToken.
  def position = Position(row, column)
  def position_=(position: Position) { row = position.row; column = position.column }
  private[json] var row = Position.Invalid.row
  private[json] var column = Position.Invalid.column
}

case class StartOfObjectEvent() extends JsonEvent
case class EndOfObjectEvent() extends JsonEvent
case class StartOfArrayEvent() extends JsonEvent
case class EndOfArrayEvent() extends JsonEvent
case class FieldEvent(name: String) extends JsonEvent
case class IdentifierEvent(text: String) extends JsonEvent
case class NumberEvent(number: BigDecimal) extends JsonEvent
case class StringEvent(string: String) extends JsonEvent
