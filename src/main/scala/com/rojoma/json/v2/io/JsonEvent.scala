package com.rojoma.json.v2
package io

sealed abstract class JsonEvent {
  def position: Position
}

case class StartOfObjectEvent()(val position: Position) extends JsonEvent
case class EndOfObjectEvent()(val position: Position) extends JsonEvent
case class StartOfArrayEvent()(val position: Position) extends JsonEvent
case class EndOfArrayEvent()(val position: Position) extends JsonEvent
case class FieldEvent(name: String)(val position: Position) extends JsonEvent
case class IdentifierEvent(text: String)(val position: Position) extends JsonEvent
case class NumberEvent(number: String)(val position: Position) extends JsonEvent
case class StringEvent(string: String)(val position: Position) extends JsonEvent
