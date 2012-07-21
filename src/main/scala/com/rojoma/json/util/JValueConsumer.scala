package com.rojoma.json
package util

import scala.annotation.tailrec

import io._
import ast.JValue

import JValueConsumer._

// End-to-end character data to JValue push parser
final class JValueConsumer private (lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator) {
  def consume(data: WrappedCharArray): SuccessfulResult = apply(data) match {
    case r: SuccessfulResult => r
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
  }

  def apply(data: WrappedCharArray): Result = lexer(data) match {
    case JsonTokenGenerator.Token(token, newLexer, remainingData) =>
      processDatum(token, newLexer, parser, valuator, remainingData)
    case JsonTokenGenerator.More(newLexer) =>
      More(new JValueConsumer(newLexer, parser, valuator))
    case e: JsonTokenGenerator.Error =>
      LexerError(e)
  }

  def endOfInput(): EndResult = {
    lexer.endOfInput() match {
      case JsonTokenGenerator.EndOfInput(r, c) =>
        parser.endOfInput(r, c) match {
          case JsonEventGenerator.Finished => UnexpectedEndOfInput(r, c)
          case JsonEventGenerator.Unfinished(r, c) => UnexpectedEndOfInput(r, c)
          case e: JsonEventGenerator.UnexpectedToken => ParserError(e)
        }
      case JsonTokenGenerator.FinalToken(t, r, c) =>
        processDatum(t, JsonTokenGenerator.newPositionedGenerator(r, c), parser, valuator, WrappedCharArray.empty) match {
          case Value(v, _) => FinalValue(v)
          case More(_) => UnexpectedEndOfInput(r, c)
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
      case UnexpectedEndOfInput(r, c) => throw new JsonEOF(r, c)
    }
  }

  @tailrec
  final def processDatum(token: PositionedJsonToken, lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator, data: WrappedCharArray): Result = {
    // At this point we know we're handing a datum.  And "token" is part of it.
    parser(token) match {
      case JsonEventGenerator.Event(ev, newParser) =>
        valuator(ev) match {
          case JValueGenerator.Value(jvalue) =>
            assert(newParser.atTopLevel)
            Value(jvalue, data)
          case JValueGenerator.More(newValuator) =>
            assert(!newParser.atTopLevel)
            lexer(data) match {
              case JsonTokenGenerator.Token(newToken, newLexer, remainingData) =>
                processDatum(newToken, newLexer, newParser, newValuator, remainingData)
              case JsonTokenGenerator.More(newLexer) =>
                More(new JValueConsumer(newLexer, newParser, newValuator))
              case e: JsonTokenGenerator.Error =>
                LexerError(e)
            }
          case _ : JValueGenerator.BadParse =>
            // this shouldn't happen
            throw new Exception("Bad parse from JValueGenerator")
          case JValueGenerator.UnknownIdentifier(i, row, col) =>
            ParserError(JsonEventGenerator.UnexpectedToken(TokenIdentifier(i), "datum", row, col))
        }
      case JsonEventGenerator.More(newParser) =>
        // yeah, this is an almost exact copy of the JValueGenerator.More case right above.  SURE WOULD BE
        // NICE if the jvm had proper tailcall elimination so this could be factored out into a sister
        // method.
        lexer(data) match {
          case JsonTokenGenerator.Token(newToken, newLexer, remainingData) =>
            processDatum(newToken, newLexer, newParser, valuator, remainingData)
          case JsonTokenGenerator.More(newLexer) =>
            More(new JValueConsumer(newLexer, newParser, valuator))
          case e: JsonTokenGenerator.Error =>
            LexerError(e)
        }
      case e: JsonEventGenerator.Error =>
        ParserError(e)
    }
  }
}

object JValueConsumer {
  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult

  case class More(nextState: JValueConsumer) extends SuccessfulResult
  case class Value(value: JValue, remainingInput: WrappedCharArray) extends SuccessfulResult

  case class FinalValue(value: JValue) extends SuccessfulEndResult

  case class LexerError(err: JsonTokenGenerator.AnyError) extends Error with EndError
  case class ParserError(err: JsonEventGenerator.AnyError) extends Error with EndError
  case class UnexpectedEndOfInput(row: Int, col: Int) extends EndError

  val newConsumer = new JValueConsumer(JsonTokenGenerator.newGenerator, JsonEventGenerator.newGenerator, JValueGenerator.newGenerator)
}
