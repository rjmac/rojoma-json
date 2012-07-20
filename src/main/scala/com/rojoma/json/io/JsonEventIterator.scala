package com.rojoma.json
package io

sealed abstract class JsonEvent
case object StartOfObjectEvent extends JsonEvent
case object EndOfObjectEvent extends JsonEvent
case object StartOfArrayEvent extends JsonEvent
case object EndOfArrayEvent extends JsonEvent
case class FieldEvent(name: String) extends JsonEvent
case class IdentifierEvent(text: String) extends JsonEvent
case class NumberEvent(number: BigDecimal) extends JsonEvent
case class StringEvent(string: String) extends JsonEvent

case class PositionedJsonEvent(event: JsonEvent, row: Int, column: Int)

class JsonEventIterator(input: Iterator[PositionedJsonToken]) extends BufferedIterator[PositionedJsonEvent] {
  private var parser = JsonEventGenerator.newGenerator
  private val underlying = input.buffered
  private var available: PositionedJsonEvent = null
  private var atTop = true // this reflects the state *before* the feed that resulted in "available" being set.

  def hasNext: Boolean = {
    if(available != null) {
      true
    } else {
      atTop = parser.atTopLevel
      while(underlying.hasNext && available == null) {
        val token = underlying.next()
        parser.parse(token) match {
          case JsonEventGenerator.Event(ev, newParser) =>
            available = ev
            parser = newParser
          case JsonEventGenerator.More(newParser) =>
            parser = newParser
        }
      }
      available != null
    }
  }

  def head = {
    if(available == null && !hasNext) {
      underlying.next() // force NoSuchElementException
    }
    available
  }

  def next() = {
    val result = head
    available = null
    result
  }

  /**
   * Finish reading the "current" object or list, where "current" is
   * defined as "the most recent compound object started by `next()`.
   * If a top-level object has not been started, this does nothing.
   *
   * Throws `JsonEOF` if the end-of-input occurs before finishing
   * this object.
   */
  def skipRestOfCompound(): this.type = {
    hasNext // hasNext to make sure atTop is in an accurate state
    if(!atTop) {
      try {
        var count = 0
        do {
          val ev = next().event
          ev match {
            case StartOfObjectEvent | StartOfArrayEvent => count += 1
            case EndOfObjectEvent | EndOfArrayEvent => count -= 1
            case _ => /* nothing */
          }
        } while(count >= 0)
      } catch {
        case NoSuchTokenException(r,c) => throw JsonEOF(r,c)
        case _: NoSuchElementException => throw JsonEOF(-1, -1)
      }
    }
    this
  }

  @inline
  final def dropRestOfCompound() = skipRestOfCompound()

  /** Skips the next datum that would be returned entirely.  If the next event
   * is the start of a list or object, `skipRestOfCompound()` is called to
   * pass over it. If it's a field event, the field and its associated value
   * are skipped. If it's the end of a list or object, no position change is
   * made and the next call to `head` or `next()` will still return the end
   * event.  Otherwise, it's an atom and is consumed.
   *
   * If the iterator is empty at the start of this call, `NoSuchElementException`
   * is raised.  If it runs out while skipping the datum, `JsonEOF` is raised.
   */
  def skipNextDatum(): this.type = head.event match {
    case StartOfObjectEvent | StartOfArrayEvent =>
      next()
      skipRestOfCompound()
    case FieldEvent(_) =>
      next()
      skipNextDatum()
    case EndOfObjectEvent | EndOfArrayEvent =>
      this
    case _ =>
      next()
      this
  }

  @inline
  final def dropNextDatum() = skipNextDatum()
}
