package com.rojoma.json.v3
package io

import ast._

private[io] object pos {
  def apply(position: Position, restOfMessage: String, restOfMessageArgs: Any*) = {
    val message = restOfMessage.format(restOfMessageArgs:_*)
    if(position.row == -1 && position.column == -1) message
    else position + ": " + message
  }
}

class NoSuchTokenException(val position: Position) extends NoSuchElementException("Empty iterator") {
  def row: Int = position.row
  def column: Int = position.column
}

sealed abstract class JsonReaderException(val message: String) extends Exception(message) {
  def position: Position
  def row: Int = position.row
  def column: Int = position.column
}

sealed trait JsonLexException extends JsonReaderException
sealed trait JsonParseException extends JsonReaderException
sealed trait JsonReadException extends JsonReaderException

class JsonUnexpectedCharacter(val character: Char, val expected: String, val position: Position) extends JsonReaderException(pos(position, "Expected %s; got character %s", expected, JString(character.toString))) with JsonLexException
class JsonNumberOutOfRange(val number: String, val position: Position) extends JsonReaderException(pos(position, "Cannot store in BigDecimal: %s", number)) with JsonLexException

sealed abstract class JsonEOF(val position: Position) extends JsonReaderException(pos(position, "Unexpected end of input"))
class JsonLexerEOF(position: Position) extends JsonEOF(position) with JsonLexException
class JsonParserEOF(position: Position) extends JsonEOF(position) with JsonParseException

class JsonUnexpectedToken(val token: JsonToken, val expected: String) extends JsonReaderException(pos(token.position, "Expected %s; got token %s", expected, token.asFragment)) with JsonParseException {
  def position = token.position
}
class JsonUnknownIdentifier(val identifier: String, val position: Position) extends JsonReaderException(pos(position, "Unknown identifier %s", JString(identifier))) with JsonParseException {
  def this(i: IdentifierEvent) = this(i.text, i.position)
}

/** This exception should never be thrown if using the standard
 * `JsonEventIterator`.  It means that either there were mismatched
 * start/end array or object events or that an an object did not
 * follow the pattern of FieldObject-followed-by-field-data. */
class JsonBadParse(val event: JsonEvent) extends JsonReaderException(pos(event.position, "Received unexpected event %s", event)) with JsonReadException {
  def position = event.position
}

// codecs -- these do not preserve stack information, since the main
// use-case I see for them is reporting errors to user code.

object JsonLexException {
  implicit val jCodec = locally {
    import matcher._
    import codec._
    import util._

    implicit val characterCodec = new JsonEncode[Char] with JsonDecode[Char] {
      def encode(c: Char) = JString(c.toString)
      def decode(x: JValue) = x match {
        case js@JString(s) =>
          if(s.length == 1) Right(s.charAt(0))
          else Left(DecodeError.InvalidValue(js))
        case other =>
          Left(DecodeError.InvalidType(expected = JString, got = other.jsonType))
      }
    }

    implicit val unexpectedCharacterCodec = new JsonEncode[JsonUnexpectedCharacter] with JsonDecode[JsonUnexpectedCharacter] {
      private val character = Variable[Char]()
      private val expected = Variable[String]()
      private val position = Variable[Position]()
      private val pattern = PObject(
        "character" -> character,
        "expected" -> expected,
        "position" -> POption(position)
      )

      def encode(e: JsonUnexpectedCharacter) = pattern.generate(
        character := e.character,
        expected := e.expected,
        position := e.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonUnexpectedCharacter(character(r), expected(r), position.getOrElse(r, Position.Invalid))
      }
    }

    implicit val numberOutOfRangeCodec = new JsonEncode[JsonNumberOutOfRange] with JsonDecode[JsonNumberOutOfRange] {
      private val number = Variable[String]()
      private val position = Variable[Position]()
      private val pattern = PObject(
        "number" -> number,
        "position" -> POption(position)
      )
      
      def encode(e: JsonNumberOutOfRange) = pattern.generate(
        number := e.number,
        position := e.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonNumberOutOfRange(number(r), position.getOrElse(r, Position.Invalid))
      }
    }

    implicit val lexerEofCodec = new JsonEncode[JsonLexerEOF] with JsonDecode[JsonLexerEOF] {
      private val position = Variable[Position]()
      private val pattern = PObject(
        "phase" -> "lexer",
        "position" -> POption(position)
      )

      def encode(e: JsonLexerEOF) = pattern.generate(
        position := e.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonLexerEOF(position.getOrElse(r, Position.Invalid))
      }
    }

    SimpleHierarchyCodecBuilder[JsonLexException](InternalTag("type", false)).
      branch[JsonUnexpectedCharacter]("unexpected-character").
      branch[JsonNumberOutOfRange]("number-out-of-range").
      branch[JsonLexerEOF]("eof").
      build
  }
}

object JsonParseException {
  implicit val jCodec = locally {
    import matcher._
    import codec._
    import util._

    implicit val unexpectedTokenCodec = new JsonEncode[JsonUnexpectedToken] with JsonDecode[JsonUnexpectedToken] {
      private val token = Variable[JsonToken](JsonToken.sansPositionCodec)
      private val expected = Variable[String]()
      private val position = Variable[Position]()
      private val pattern = PObject(
        "token" -> token,
        "expected" -> expected,
        "position" -> POption(position)
      )

      def encode(e: JsonUnexpectedToken) = pattern.generate(
        token := e.token,
        expected := e.expected,
        position := e.token.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonUnexpectedToken(token(r).positionedAt(position.getOrElse(r, Position.Invalid)), expected(r))
      }
    }

    implicit val unknownIdentifierCodec = new JsonEncode[JsonUnknownIdentifier] with JsonDecode[JsonUnknownIdentifier] {
      private val identifier = Variable[String]()
      private val position = Variable[Position]()
      private val pattern = PObject(
        "identifier" -> identifier,
        "position" -> POption(position)
      )

      def encode(e: JsonUnknownIdentifier) = pattern.generate(
        identifier := e.identifier,
        position := e.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonUnknownIdentifier(identifier(r), position.getOrElse(r, Position.Invalid))
      }
    }

    implicit val parserEofCodec = new JsonEncode[JsonParserEOF] with JsonDecode[JsonParserEOF] {
      private val position = Variable[Position]()
      private val pattern = PObject(
        "phase" -> "parser",
        "position" -> POption(position)
      )

      def encode(e: JsonParserEOF) = pattern.generate(
        position := e.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonParserEOF(position.getOrElse(r, Position.Invalid))
      }
    }

    SimpleHierarchyCodecBuilder[JsonParseException](InternalTag("type", false)).
      branch[JsonUnexpectedToken]("unexpected-token").
      branch[JsonUnknownIdentifier]("unknown-identifier").
      branch[JsonParserEOF]("eof").
      build
  }
}

object JsonReadException {
  implicit val jCodec = locally {
    import matcher._
    import codec._
    import util._

    implicit val badParseCodec = new JsonEncode[JsonBadParse] with JsonDecode[JsonBadParse] {
      private val event = Variable[JsonEvent](JsonEvent.sansPositionCodec)
      private val position = Variable[Position]()
      private val pattern = PObject(
        "event" -> event,
        "position" -> POption(position)
      )

      def encode(e: JsonBadParse) = pattern.generate(
        event := e.event,
        position := e.event.position
      )

      def decode(x: JValue) = pattern.matches(x).right.map { r =>
        new JsonBadParse(event(r).positionedAt(position.getOrElse(r, Position.Invalid)))
      }
    }

    SimpleHierarchyCodecBuilder[JsonReadException](InternalTag("type", false)).
      branch[JsonBadParse]("bad-parse").
      build
  }
}

object JsonReaderException {
    import codec._

  implicit val jCodec: JsonEncode[JsonReaderException] with JsonDecode[JsonReaderException] = new JsonEncode[JsonReaderException] with JsonDecode[JsonReaderException] {
    private val Type = Path("type")
    private val Phase = Path("phase")

    def encode(e: JsonReaderException) = e match {
      case l: JsonLexException => JsonEncode.toJValue(l)
      case p: JsonParseException => JsonEncode.toJValue(p)
      case r: JsonReadException => JsonEncode.toJValue(r)
    }

    def decode(x: JValue) =
      JsonDecode.fromJValue[JsonLexException](x) match {
        case r@Right(_) => r
        case Left(DecodeError.InvalidValue(_, Type)) | Left(DecodeError.InvalidValue(JString("parser"), Phase)) =>
          JsonDecode.fromJValue[JsonParseException](x) match {
            case r@Right(_) => r
            case Left(DecodeError.InvalidValue(_, Type)) =>
              JsonDecode.fromJValue[JsonReadException](x)
            case l@Left(_) => l
          }
        case l@Left(_) => l
      }
  }
}
