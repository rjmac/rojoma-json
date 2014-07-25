package com.rojoma.json.v3
package io

sealed abstract class JsonEvent {
  val position: Position

  def unpositioned: JsonEvent // This is abstract so evt.unpositioned can retain its type
  def positionedAt(newPos: Position): JsonEvent
}

case class StartOfObjectEvent()(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = StartOfObjectEvent()(newPos)
}

case class EndOfObjectEvent()(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = EndOfObjectEvent()(newPos)
}

case class StartOfArrayEvent()(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = StartOfArrayEvent()(newPos)
}

case class EndOfArrayEvent()(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = EndOfArrayEvent()(newPos)
}

case class FieldEvent(name: String)(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = FieldEvent(name)(newPos)
}

case class IdentifierEvent(text: String)(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = IdentifierEvent(text)(newPos)
  def isValid = ReaderUtils.isValidIdentifier(text)
}

case class NumberEvent(number: String)(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = NumberEvent(number)(newPos)
  def isValid = ReaderUtils.isValidNumber(number)
}

case class StringEvent(string: String)(val position: Position) extends JsonEvent {
  def unpositioned: JsonEvent = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = StringEvent(string)(newPos)
}

object JsonEvent {
  import ast._
  import codec._

  private val StartOfObjectTag = JString("start of object")
  private val EndOfObjectTag = JString("end of object")
  private val StartOfArrayTag = JString("start of array")
  private val EndOfArrayTag = JString("end of array")
  private val FieldTag = JString("field")
  private val IdentifierTag = JString("identifier")
  private val NumberTag = JString("number")
  private val StringTag = JString("string")

  val sansPositionCodec: JsonEncode[JsonEvent] with JsonDecode[JsonEvent] = new JsonEncode[JsonEvent] with JsonDecode[JsonEvent] {
    private val Type = "type"
    private val Value = "value"

    def encode(e: JsonEvent) = e match {
      case StartOfObjectEvent() => JObject(Map(Type -> StartOfObjectTag))
      case EndOfObjectEvent() => JObject(Map(Type -> EndOfObjectTag))
      case StartOfArrayEvent() => JObject(Map(Type -> StartOfArrayTag))
      case EndOfArrayEvent() => JObject(Map(Type -> EndOfArrayTag))
      case FieldEvent(name) => JObject(Map(Type -> FieldTag, Value -> JString(name)))
      case IdentifierEvent(name) => JObject(Map(Type -> IdentifierTag, Value -> JString(name)))
      case NumberEvent(num) => JObject(Map(Type -> NumberTag, Value -> JString(num)))
      case StringEvent(str) => JObject(Map(Type -> StringTag, Value -> JString(str)))
    }

    private def extractValue[T](o: JObject, wrap: String => T)(validate: T => Boolean = Function.const(true) _) =
      o.get(Value) match {
        case Some(js@JString(s)) =>
          val res = wrap(s)
          if(validate(res)) Right(res)
          else Left(DecodeError.InvalidValue(js, Path(Value)))
        case Some(other) => Left(DecodeError.InvalidType(expected = JString, got = other.jsonType, path = Path(Value)))
        case None => Left(DecodeError.MissingField(Value))
      }

    def decode(x: JValue) = x match {
      case o: JObject =>
        o.get(Type) match {
          case Some(StartOfObjectTag) => Right(StartOfObjectEvent()(Position.Invalid))
          case Some(EndOfObjectTag) => Right(EndOfObjectEvent()(Position.Invalid))
          case Some(StartOfArrayTag) => Right(StartOfArrayEvent()(Position.Invalid))
          case Some(EndOfArrayTag) => Right(EndOfArrayEvent()(Position.Invalid))
          case Some(FieldTag) => extractValue(o, FieldEvent(_)(Position.Invalid))()
          case Some(IdentifierTag) => extractValue(o, IdentifierEvent(_)(Position.Invalid))(_.isValid)
          case Some(NumberTag) => extractValue(o, NumberEvent(_)(Position.Invalid))(_.isValid)
          case Some(StringTag) => extractValue(o, StringEvent(_)(Position.Invalid))()
          case Some(other : JString) => Left(DecodeError.InvalidValue(other, Path(Type)))
          case Some(other) => Left(DecodeError.InvalidType(expected = JString, got = other.jsonType, path = Path(Type)))
          case None => Left(DecodeError.MissingField(Type))
        }
      case other =>
        Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
    }
  }

  implicit val jCodec: JsonEncode[JsonEvent] with JsonDecode[JsonEvent] = new JsonEncode[JsonEvent] with JsonDecode[JsonEvent] {
    private val Pos = "position"

    def encode(e: JsonEvent) = {
      val JObject(fields) = sansPositionCodec.encode(e)
      JObject(fields + (Pos -> JsonEncode.toJValue(e.position)))
    }

    def decode(x: JValue) = x match {
      case obj: JObject =>
        sansPositionCodec.decode(obj).right.flatMap { token =>
          obj.get(Pos) match {
            case Some(jsonPos) =>
              JsonDecode.fromJValue[Position](jsonPos) match {
                case Right(pos) => Right(token.positionedAt(pos))
                case Left(err) => Left(err.prefix(Pos))
              }
            case None => // no position is OK
              Right(token)
          }
        }
      case other =>
        Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
    }
  }
}
