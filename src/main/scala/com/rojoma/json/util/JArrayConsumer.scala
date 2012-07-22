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
  case class UnexpectedEndOfInput(finalValue: Option[JValue], row: Int, column: Int) extends EndError

  val newConsumer: JArrayConsumer = new AwaitingStartOfList(JsonTokenGenerator.newGenerator)

  def throwError(e: AnyError): Nothing = e match {
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
    case UnexpectedEndOfInput(_, r, c) => throw new JsonEOF(r, c)
  }
}

private[util] object JArrayConsumerImpl {
  case class AwaitingStartOfList(lexer: JsonTokenGenerator) extends JArrayConsumer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenOpenBracket, _, _), newLexer, remainingData) =>
        val newState = new AwaitingDatumOrEndOfList(newLexer)
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
        UnexpectedEndOfInput(None, r, c)
      case JsonTokenGenerator.FinalToken(token, _, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token.token, "start of list", token.row, token.column))
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(None, r, c)
    }
  }

  case class AwaitingDatumOrEndOfList(lexer: JsonTokenGenerator) extends JArrayConsumer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenCloseBracket, _, _), _, remainingData) =>
        EndOfList(remainingData)
      case JsonTokenGenerator.Token(_, _, _) =>
        new AwaitingDatum(lexer).apply(data)
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingDatumOrEndOfList(newLexer))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenCloseBracket, _, _), _, _) =>
        FinalEndOfList
      case JsonTokenGenerator.FinalToken(_, _, _) =>
        new AwaitingDatum(lexer).endOfInput()
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(None, r, c)
    }
  }

  case class AwaitingCommaOrEndOfList(lexer: JsonTokenGenerator) extends JArrayConsumer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenComma, _, _), newLexer, remainingData) =>
        new AwaitingDatum(newLexer).apply(remainingData)
      case JsonTokenGenerator.Token(PositionedJsonToken(TokenCloseBracket, _, _), _, remainingData) =>
        EndOfList(remainingData)
      case JsonTokenGenerator.Token(token, _, remainingData) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token.token, "comma or end of list", token.row, token.column))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingCommaOrEndOfList(newLexer))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenCloseBracket, _, _), _, _) =>
        FinalEndOfList
      case JsonTokenGenerator.FinalToken(PositionedJsonToken(TokenComma, _, _), r, c) =>
        UnexpectedEndOfInput(None, r, c)
      case JsonTokenGenerator.FinalToken(_, _, _) =>
        new AwaitingDatum(lexer).endOfInput()
      case JsonTokenGenerator.EndOfInput(r, c) =>
        UnexpectedEndOfInput(None, r, c)
    }
  }

  case class AwaitingDatum(valueConsumer: JValueConsumer) extends JArrayConsumer {
    def this(lexer: JsonTokenGenerator) = this(JValueConsumer.newConsumerFromLexer(lexer))

    def apply(data: WrappedCharArray): Result = valueConsumer(data) match {
      case JValueConsumer.More(newState) => More(new AwaitingDatum(newState))
      case JValueConsumer.Value(jvalue, newLexer, remainingData) =>
        Element(jvalue, new AwaitingCommaOrEndOfList(newLexer), remainingData)
      case JValueConsumer.LexerError(err) => LexerError(err)
      case JValueConsumer.ParserError(err) => ParserError(err)
    }

    def endOfInput() = valueConsumer.endOfInput() match {
      case JValueConsumer.FinalValue(value, r, c) =>
        UnexpectedEndOfInput(Some(value), r, c)
      case JValueConsumer.UnexpectedEndOfInput(r, c) =>
        UnexpectedEndOfInput(None, r, c)
      case JValueConsumer.LexerError(e) =>
        LexerError(e)
      case JValueConsumer.ParserError(e) =>
        ParserError(e)
    }
  }
}
