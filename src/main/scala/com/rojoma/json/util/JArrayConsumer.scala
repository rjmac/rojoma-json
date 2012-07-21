package com.rojoma.json
package util

import scala.annotation.tailrec

import io._
import ast.JValue

import JArrayConsumer._
import JArrayConsumerImpl._

// Incremental parsing of JSON lists
abstract class JArrayConsumer {
  def apply(data: WrappedCharArray): Result

  def consume(data: WrappedCharArray): SuccessfulResult = apply(data) match {
    case r: SuccessfulResult => r
    case e: Error => throwError(e)
  }

  def endOfInput(): EndResult

  def finish(): SuccessfulEndResult = endOfInput() match {
    case r: SuccessfulEndResult => r
    case e: EndError => throwError(e)
  }
}

object JArrayConsumer {
  sealed trait AnyError

  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result with AnyError

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult with AnyError

  case class EndOfList(remainingInput: WrappedCharArray) extends SuccessfulResult
  case class More(nextState: JArrayConsumer) extends SuccessfulResult
  case class Element(value: JValue, nextState: JArrayConsumer, remainingInput: WrappedCharArray) extends SuccessfulResult

  case object FinalEndOfList extends SuccessfulEndResult

  case class LexerError(err: JsonTokenGenerator.AnyError) extends Error with EndError
  case class ParserError(err: JsonEventGenerator.AnyError) extends Error with EndError
  case class UnexpectedEndOfInput(row: Int, column: Int) extends EndError

  val newConsumer: JArrayConsumer = new AwaitingStartOfList(JsonTokenGenerator.newGenerator)

  def throwError(e: AnyError): Nothing = e match {
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
    case UnexpectedEndOfInput(r, c) => throw new JsonEOF(r, c)
  }
}

private[util] object JArrayConsumerImpl {
  case class AwaitingStartOfList(lexer: JsonTokenGenerator) extends JArrayConsumer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenOpenBracket, _, _), newLexer, remainingData) =>
        val newState = new AwaitingDatumOrEndOfList(newLexer, JsonEventGenerator.newGenerator)
        newState(remainingData)
      case JsonTokenGenerator.Token(token, _, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token.token, "start of list", token.row, token.column))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingStartOfList(newLexer))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenOpenBracket, _, _), r, c) =>
        UnexpectedEndOfInput(r, c)
      case JsonTokenGenerator.FinalToken(token, _, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token.token, "start of list", token.row, token.column))
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(r, c)
    }
  }

  abstract class DatumProcessing extends JArrayConsumer {
    @tailrec
    final def processDatum(token: PositionedJsonToken, lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator, data: WrappedCharArray): Result = {
      // At this point we know we're handing a datum.  And "token" is part of it.
      parser(token) match {
        case JsonEventGenerator.Event(ev, newParser) =>
          valuator(ev) match {
            case JValueGenerator.Value(jvalue) =>
              assert(newParser.atTopLevel)
              Element(jvalue, new AwaitingCommaOrEndOfList(lexer, newParser), data)
            case JValueGenerator.More(newValuator) =>
              assert(!newParser.atTopLevel)
              lexer(data) match {
                case JsonTokenGenerator.Token(newToken, newLexer, remainingData) =>
                  processDatum(newToken, newLexer, newParser, newValuator, remainingData)
                case JsonTokenGenerator.More(newLexer) =>
                  More(new AwaitingDatum(newLexer, newParser, newValuator))
                case e: JsonTokenGenerator.Error =>
                  LexerError(e)
              }
            case _ :JValueGenerator.BadParse =>
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
              More(new AwaitingDatum(newLexer, newParser, valuator))
            case e: JsonTokenGenerator.Error =>
              LexerError(e)
          }
        case e: JsonEventGenerator.Error =>
          ParserError(e)
      }
    }
  }

  case class AwaitingDatumOrEndOfList(lexer: JsonTokenGenerator, parser: JsonEventGenerator) extends DatumProcessing {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenCloseBracket, _, _), _, remainingData) =>
        EndOfList(remainingData)
      case JsonTokenGenerator.Token(token, newLexer, remainingData) =>
        processDatum(token, newLexer, parser, JValueGenerator.newGenerator, remainingData)
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingDatumOrEndOfList(newLexer, parser))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenCloseBracket, _, _), _, _) =>
        FinalEndOfList
      case JsonTokenGenerator.FinalToken(token, r, c) =>
        parser(token) match {
          case JsonEventGenerator.Event(_, _) => UnexpectedEndOfInput(r, c) // hm, should we feed it to a JValueGenerator first, knowing that this list is incomplete?
          case JsonEventGenerator.More(_) => UnexpectedEndOfInput(r, c)
          case e: JsonEventGenerator.Error => ParserError(e)
        }
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(r, c)
    }
  }

  case class AwaitingCommaOrEndOfList(lexer: JsonTokenGenerator, parser: JsonEventGenerator) extends JArrayConsumer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenComma, _, _), newLexer, remainingData) =>
        val newState = new AwaitingDatum(newLexer, parser, JValueGenerator.newGenerator)
        newState(remainingData)
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenCloseBracket, _, _), _, remainingData) =>
        EndOfList(remainingData)
      case JsonTokenGenerator.Token(token, _, remainingData) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token.token, "comma or end of list", token.row, token.column))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingCommaOrEndOfList(newLexer, parser))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenCloseBracket, _, _), _, _) =>
        FinalEndOfList
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenComma, _, _), r, c) =>
        UnexpectedEndOfInput(r, c)
      case JsonTokenGenerator.FinalToken(token, r, c) =>
        parser(token) match {
          case JsonEventGenerator.Event(_, _) => UnexpectedEndOfInput(r, c) // hm, should we feed it to a JValueGenerator first, knowing that this list is incomplete?
          case JsonEventGenerator.More(_) => UnexpectedEndOfInput(r, c)
          case e: JsonEventGenerator.Error => ParserError(e)
        }
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(r, c)
    }
  }

  case class AwaitingDatum(lexer: JsonTokenGenerator, parser: JsonEventGenerator, valuator: JValueGenerator) extends DatumProcessing {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(token, newLexer, remainingData) =>
        processDatum(token, newLexer, parser, valuator, remainingData)
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingDatum(newLexer, parser, valuator))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(token, r, c) =>
        parser(token) match {
          case JsonEventGenerator.Event(_, _) => UnexpectedEndOfInput(r, c) // hm, should we feed it to a JValueGenerator first, knowing that this list is incomplete?
          case JsonEventGenerator.More(_) => UnexpectedEndOfInput(r, c)
          case e: JsonEventGenerator.Error => ParserError(e)
        }
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(r, c)
    }
  }
}
