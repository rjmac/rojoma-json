package com.rojoma.json
package io

import scala.collection.mutable.LinkedHashMap

import ast._

import JValueGenerator._

sealed abstract class JValueGenerator {
  def apply(event: PositionedJsonEvent): Result
}

object JValueGenerator {
  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  case class Value(value: JValue) extends SuccessfulResult
  case class More(newState: JValueGenerator) extends SuccessfulResult
  sealed abstract class Error extends Result {
    def row: Int
    def column: Int
  }
  case class UnknownIdentifier(identifier: String, row: Int, column: Int) extends Error
  case class BadParse(event: JsonEvent, row: Int, column: Int) extends Error

  val newGenerator: JValueGenerator = JValueGeneratorImpl.AwaitingStartOfDatum
}

private[io] object JValueGeneratorImpl {
  final case class Field(key: String, value: JValue)

  val jtrue = JBoolean(true)
  val jfalse = JBoolean(false)

  sealed abstract class ChildState extends JValueGenerator {
    protected def parent: ParentState

    protected def result(v: JValue): Result =
      if(parent == null) Value(v)
      else parent.add(v)

    protected def enparent(s: String) = if(parent == null) s else s + " :: " + parent

    def jObject(fields: Vector[Field]) = {
      val map = new LinkedHashMap[String, JValue]
      val it = fields.iterator
      while(it.hasNext) {
        val f = it.next()
        map(f.key) = f.value
      }
      JObject(map)
    }
  }

  sealed abstract class ParentState extends ChildState { // a child which can also be a parent
    def add(value: JValue): Result

    protected def commonHandle(event: PositionedJsonEvent): Result = event.event match {
      case StartOfObjectEvent => More(new AwaitingKeyOrEndOfObject(Vector.empty, this))
      case StartOfArrayEvent => More(new AwaitingElementOrEndOfArray(Vector.empty, this))
      case StringEvent(s) => add(JString(s))
      case NumberEvent(n) => add(JNumber(n))
      case IdentifierEvent("true") => add(jtrue)
      case IdentifierEvent("false") => add(jfalse)
      case IdentifierEvent("null") => add(JNull)
      case IdentifierEvent(other) => UnknownIdentifier(other, event.row, event.column)
      case other => BadParse(other, event.row, event.column)
    }
  }

  object AwaitingStartOfDatum extends JValueGenerator {
    def apply(event: PositionedJsonEvent) =
      event.event match {
        case StartOfObjectEvent => topLevelObject
        case StartOfArrayEvent => topLevelArray
        case StringEvent(s) => Value(JString(s))
        case NumberEvent(n) => Value(JNumber(n))
        case IdentifierEvent("true") => Value(jtrue)
        case IdentifierEvent("false") => Value(jfalse)
        case IdentifierEvent("null") => Value(JNull)
        case IdentifierEvent(other) => UnknownIdentifier(other, event.row, event.column)
        case other => BadParse(other, event.row, event.column)
      }

    val topLevelObject = More(new AwaitingKeyOrEndOfObject(Vector.empty, null))
    val topLevelArray = More(new AwaitingElementOrEndOfArray(Vector.empty, null))

    override def toString = "AwaitingStartOfDatum"
  }

  class AwaitingElementOrEndOfArray(val elems: Vector[JValue], val parent: ParentState) extends ParentState {
    def apply(event: PositionedJsonEvent) =
      event.event match {
        case EndOfArrayEvent => result(JArray(elems))
        case _ => commonHandle(event)
      }

    def add(v: JValue): Result =
      More(new AwaitingElementOrEndOfArray(elems :+ v, parent))

    override def toString = enparent("AwaitingElementOrEndOfArray(" + JArray(elems) + ")")
    override def hashCode = elems.hashCode + parent.##
    override def equals(o: Any) = o match {
      case that: AwaitingElementOrEndOfArray => this.elems == that.elems && this.parent == that.parent
      case _ => false
    }
  }

  class AwaitingKeyOrEndOfObject(val fields: Vector[Field], val parent: ParentState) extends ChildState {
    def apply(event: PositionedJsonEvent) =
      event.event match {
        case FieldEvent(name) => More(new AwaitingValue(name, fields, parent))
        case EndOfObjectEvent => result(jObject(fields))
        case other => BadParse(other, event.row, event.column)
      }

    override def toString = enparent("AwaitingKeyOrEndOfObject(" + jObject(fields) + ")")
    override def hashCode = fields.hashCode + parent.##
    override def equals(o: Any) = o match {
      case that: AwaitingKeyOrEndOfObject => this.fields == that.fields && this.parent == that.parent
      case _ => false
    }
  }

  class AwaitingValue(val field: String, val fields: Vector[Field], val parent: ParentState) extends ParentState {
    def apply(event: PositionedJsonEvent) = commonHandle(event)

    def add(v: JValue): Result =
      More(new AwaitingKeyOrEndOfObject(fields :+ Field(field, v), parent))

    override def toString = enparent("AwaitingValue(" + JString(field) + " : " + jObject(fields) + ")")
    override def hashCode = field.hashCode + fields.hashCode + parent.##
    override def equals(o: Any) = o match {
      case that: AwaitingValue => this.field == that.field && this.fields == that.fields && this.parent == that.parent
      case _ => false
    }
  }
}
