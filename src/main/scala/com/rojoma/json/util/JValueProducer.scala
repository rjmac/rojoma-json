package com.rojoma.json
package util

import scala.annotation.tailrec

import io._
import ast.JValue

import JValueProducer._

// End-to-end character data to JValue push parser
final class JValueProducer private (lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator) {
  def consume(data: WrappedCharArray): SuccessfulResult = apply(data) match {
    case r: SuccessfulResult => r
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
  }

  def apply(data: WrappedCharArray): Result = lexer(data) match {
    case JsonTokenGenerator.Token(token, newLexer, remainingData) =>
      processDatum(token, newLexer, parser, valuator, remainingData)
    case JsonTokenGenerator.More(newLexer) =>
      More(new JValueProducer(newLexer, parser, valuator))
    case e: JsonTokenGenerator.Error =>
      LexerError(e)
  }

  def endOfInput(): EndResult = {
    lexer.endOfInput() match {
      case JsonTokenGenerator.EndOfInput(pos) =>
        parser.endOfInput(pos) match {
          case JsonEventGenerator.Finished => UnexpectedEndOfInput(pos)
          case JsonEventGenerator.Unfinished(endPos) => UnexpectedEndOfInput(endPos)
          case e: JsonEventGenerator.UnexpectedToken => ParserError(e)
        }
      case JsonTokenGenerator.FinalToken(t, pos) =>
        processDatum(t, JsonTokenGenerator.newPositionedGenerator(pos), parser, valuator, WrappedCharArray.empty) match {
          case Value(v, _, _) => FinalValue(v, pos)
          case More(_) => UnexpectedEndOfInput(pos)
          case e: LexerError => e
          case e: ParserError => e
        }
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
    }
  }

  def finish(): SuccessfulEndResult = {
    endOfInput() match {
      case r: SuccessfulEndResult => r
      case ParserError(e) => JsonEventGenerator.throwError(e)
      case LexerError(e) => JsonTokenGenerator.throwError(e)
      case UnexpectedEndOfInput(p) => throw new JsonParserEOF(p)
    }
  }

  @tailrec
  final def processDatum(token: JsonToken, lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator, data: WrappedCharArray): Result = {
    // At this point we know we're handing a datum.  And "token" is part of it.
    parser(token) match {
      case JsonEventGenerator.Event(ev, newParser) =>
        valuator(ev) match {
          case JValueGenerator.Value(jvalue) =>
            assert(newParser.atTopLevel)
            Value(jvalue, lexer, data)
          case JValueGenerator.More(newValuator) =>
            assert(!newParser.atTopLevel)
            lexer(data) match {
              case JsonTokenGenerator.Token(newToken, newLexer, remainingData) =>
                processDatum(newToken, newLexer, newParser, newValuator, remainingData)
              case JsonTokenGenerator.More(newLexer) =>
                More(new JValueProducer(newLexer, newParser, newValuator))
              case e: JsonTokenGenerator.Error =>
                LexerError(e)
            }
          case _ : JValueGenerator.BadParse =>
            // this shouldn't happen
            throw new Exception("Bad parse from JValueGenerator")
          case JValueGenerator.UnknownIdentifier(i, pos) =>
            val identToken = TokenIdentifier(i)
            identToken.position = pos
            ParserError(JsonEventGenerator.UnexpectedToken(identToken, "datum"))
        }
      case JsonEventGenerator.More(newParser) =>
        // yeah, this is an almost exact copy of the JValueGenerator.More case right above.  SURE WOULD BE
        // NICE if the jvm had proper tailcall elimination so this could be factored out into a sister
        // method.
        lexer(data) match {
          case JsonTokenGenerator.Token(newToken, newLexer, remainingData) =>
            processDatum(newToken, newLexer, newParser, valuator, remainingData)
          case JsonTokenGenerator.More(newLexer) =>
            More(new JValueProducer(newLexer, newParser, valuator))
          case e: JsonTokenGenerator.Error =>
            LexerError(e)
        }
      case e: JsonEventGenerator.Error =>
        ParserError(e)
    }
  }
 }

object JValueProducer {
  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult

  case class More(nextState: JValueProducer) extends SuccessfulResult
  case class Value(value: JValue, tokenGenerator: JsonTokenGenerator, remainingInput: WrappedCharArray) extends SuccessfulResult

  case class FinalValue(value: JValue, position: Position) extends SuccessfulEndResult

  case class LexerError(err: JsonTokenGenerator.AnyError) extends Error with EndError
  case class ParserError(err: JsonEventGenerator.AnyError) extends Error with EndError
  case class UnexpectedEndOfInput(position: Position) extends EndError

  @deprecated(message = "use JValueProducer.Builder")
  def newProducerFromLexer(lexer: JsonTokenGenerator) = new JValueProducer(lexer, JsonEventGenerator.newGenerator, JValueGenerator.newGenerator)

  @deprecated(message = "use JValueProducer.Builder")
  def newProducer = newProducerFromLexer(JsonTokenGenerator.newGenerator)

  class Builder private (lexer: JsonTokenGenerator, fieldCache: FieldCache) {
    def this() = this(null, null)

    private def copy(
      lexer: JsonTokenGenerator = this.lexer,
      fieldCache: FieldCache = this.fieldCache
    ): Builder =
      new Builder(lexer, fieldCache)

    def withLexer(lexer: JsonTokenGenerator) = copy(lexer = lexer)
    def withFieldCache(fieldCache: FieldCache) = copy(fieldCache = fieldCache)

    def build = {
      val trueLexer = if(lexer == null) JsonTokenGenerator.newGenerator else lexer
      val trueFieldCache = if(fieldCache == null) IdentityFieldCache else fieldCache
      new JValueProducer(trueLexer, JsonEventGenerator.newGenerator(trueFieldCache), JValueGenerator.newGenerator)
    }
  }
}
