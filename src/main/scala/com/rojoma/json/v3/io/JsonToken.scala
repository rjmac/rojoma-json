package com.rojoma.json.v3
package io

import ast._

sealed abstract class JsonToken {
  def asFragment: String
  def asMeaning: String

  val position: Position

  def unpositioned: JsonToken // this is abstract so tkn.unpositioned can retain its type
  def positionedAt(newPos: Position): JsonToken
}

sealed abstract class SimpleJsonToken(val asFragment: String, val asMeaning: String) extends JsonToken

case class TokenOpenBrace()(val position: Position) extends SimpleJsonToken("{", "open brace") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenOpenBrace()(newPos)
}

case class TokenCloseBrace()(val position: Position) extends SimpleJsonToken("}", "close brace") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenCloseBrace()(newPos)
}

case class TokenOpenBracket()(val position: Position) extends SimpleJsonToken("[", "open bracket") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenOpenBracket()(newPos)
}

case class TokenCloseBracket()(val position: Position) extends SimpleJsonToken("]", "close bracket") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenCloseBracket()(newPos)
}

case class TokenColon()(val position: Position) extends SimpleJsonToken(":", "colon") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenColon()(newPos)
}

case class TokenComma()(val position: Position) extends SimpleJsonToken(",", "comma") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenComma()(newPos)
}

case class TokenIdentifier(text: String)(val position: Position) extends SimpleJsonToken(text, "identifier") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenIdentifier(text)(newPos)
  def isValid = ReaderUtils.isValidIdentifier(text)
}

case class TokenNumber(number: String)(val position: Position) extends SimpleJsonToken(number, "number") {
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenNumber(number)(newPos)
  def isValid = ReaderUtils.isValidNumber(number)
}

case class TokenString(text: String)(val position: Position) extends JsonToken {
  lazy val asFragment = CompactJsonWriter.toString(JString(text))
  def asMeaning = "string"
  def unpositioned = positionedAt(Position.Invalid)
  def positionedAt(newPos: Position) = TokenString(text)(newPos)
}


object JsonToken {
  import ast._
  import codec._

  private val OpenBraceTag = JString("start of object")
  private val CloseBraceTag = JString("end of object")
  private val OpenBracketTag = JString("start of array")
  private val CloseBracketTag = JString("end of array")
  private val ColonTag = JString("colon")
  private val CommaTag = JString("comma")
  private val IdentifierTag = JString("identifier")
  private val NumberTag = JString("number")
  private val StringTag = JString("string")

  val sansPositionCodec: JsonEncode[JsonToken] with JsonDecode[JsonToken] = new JsonEncode[JsonToken] with JsonDecode[JsonToken] {
    private val Type = "type"
    private val Value = "value"

    def encode(e: JsonToken) = e match {
      case TokenOpenBrace() => JObject(Map(Type -> OpenBraceTag))
      case TokenCloseBrace() => JObject(Map(Type -> CloseBraceTag))
      case TokenOpenBracket() => JObject(Map(Type -> OpenBracketTag))
      case TokenCloseBracket() => JObject(Map(Type -> CloseBracketTag))
      case TokenColon() => JObject(Map(Type -> ColonTag))
      case TokenComma() => JObject(Map(Type -> CommaTag))
      case TokenIdentifier(name) => JObject(Map(Type -> IdentifierTag, Value -> JString(name)))
      case TokenNumber(num) => JObject(Map(Type -> NumberTag, Value -> JString(num)))
      case TokenString(str) => JObject(Map(Type -> StringTag, Value -> JString(str)))
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
          case Some(OpenBraceTag) => Right(TokenOpenBrace()(Position.Invalid))
          case Some(CloseBraceTag) => Right(TokenCloseBrace()(Position.Invalid))
          case Some(OpenBracketTag) => Right(TokenOpenBracket()(Position.Invalid))
          case Some(CloseBracketTag) => Right(TokenCloseBracket()(Position.Invalid))
          case Some(ColonTag) => Right(TokenColon()(Position.Invalid))
          case Some(CommaTag) => Right(TokenComma()(Position.Invalid))
          case Some(IdentifierTag) => extractValue(o, TokenIdentifier(_)(Position.Invalid))(_.isValid)
          case Some(NumberTag) => extractValue(o, TokenNumber(_)(Position.Invalid))(_.isValid)
          case Some(StringTag) => extractValue(o, TokenString(_)(Position.Invalid))()
          case Some(other : JString) => Left(DecodeError.InvalidValue(other, Path(Type)))
          case Some(other) => Left(DecodeError.InvalidType(expected = JString, got = other.jsonType, path = Path(Type)))
          case None => Left(DecodeError.MissingField(Type))
        }
      case other =>
        Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
    }
  }

  implicit val jCodec: JsonEncode[JsonToken] with JsonDecode[JsonToken] = new JsonEncode[JsonToken] with JsonDecode[JsonToken] {
    private val Pos = "position"

    def encode(e: JsonToken) = {
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
