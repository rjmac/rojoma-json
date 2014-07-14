package com.rojoma.json.v3
package io

object Events {
  def startOfArrayEvent() = StartOfArrayEvent()(Position.Invalid)
  def endOfArrayEvent() = EndOfArrayEvent()(Position.Invalid)
  def startOfObjectEvent() = StartOfObjectEvent()(Position.Invalid)
  def endOfObjectEvent() = EndOfObjectEvent()(Position.Invalid)
  def fieldEvent(s: String) = FieldEvent(s)(Position.Invalid)
  def stringEvent(s: String) = StringEvent(s)(Position.Invalid)
  def identifierEvent(s: String) = IdentifierEvent(s)(Position.Invalid)
  def numberEvent(s: String) = NumberEvent(s)(Position.Invalid)
  def numberEvent(i: Int) = NumberEvent(i.toString)(Position.Invalid)
}
