package com.rojoma.json
package util

import scala.annotation.tailrec

import io._
import ast.JValue

import JArrayProducer._
import JArrayProducerImpl._

// Incremental parsing of JSON lists
abstract class JArrayProducer {
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

object JArrayProducer {
  sealed trait AnyError

  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result with AnyError

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult with AnyError

  case class EndOfList(tokenGenerator: JsonTokenGenerator, remainingInput: WrappedCharArray) extends SuccessfulResult
  case class More(nextState: JArrayProducer) extends SuccessfulResult
  case class Element(value: JValue, nextState: JArrayProducer, remainingInput: WrappedCharArray) extends SuccessfulResult

  case class FinalEndOfList(position: Position) extends SuccessfulEndResult

  case class LexerError(err: JsonTokenGenerator.AnyError) extends Error with EndError
  case class ParserError(err: JsonEventGenerator.AnyError) extends Error with EndError
  case class UnexpectedEndOfInput(finalValue: Option[JValue], endPosition: Position) extends EndError

  def newProducerFromLexer(lexer: JsonTokenGenerator): JArrayProducer = new AwaitingStartOfList(lexer)
  val newProducer = newProducerFromLexer(JsonTokenGenerator.newGenerator)

  def newProducerAfterStartOfList(lexer: JsonTokenGenerator): JArrayProducer = new AwaitingDatumOrEndOfList(lexer)

  def throwError(e: AnyError): Nothing = e match {
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
    case UnexpectedEndOfInput(_, pos) => throw new JsonParserEOF(pos)
  }
}

private[util] object JArrayProducerImpl {
  case class AwaitingStartOfList(lexer: JsonTokenGenerator) extends JArrayProducer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenOpenBracket(), newLexer, remainingData) =>
        val newState = new AwaitingDatumOrEndOfList(newLexer)
        newState(remainingData)
      case JsonTokenGenerator.Token(token, _, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token, "start of list"))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingStartOfList(newLexer))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(TokenOpenBracket(), pos) =>
        UnexpectedEndOfInput(None, pos)
      case JsonTokenGenerator.FinalToken(token, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token, "start of list"))
      case JsonTokenGenerator.EndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
    }
  }

  case class AwaitingDatumOrEndOfList(lexer: JsonTokenGenerator) extends JArrayProducer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenCloseBracket(), newLexer, remainingData) =>
        EndOfList(newLexer, remainingData)
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
      case JsonTokenGenerator.FinalToken(TokenCloseBracket(), pos) =>
        FinalEndOfList(pos)
      case JsonTokenGenerator.FinalToken(_, _) =>
        new AwaitingDatum(lexer).endOfInput()
      case JsonTokenGenerator.EndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
    }
  }

  case class AwaitingCommaOrEndOfList(lexer: JsonTokenGenerator) extends JArrayProducer {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenComma(), newLexer, remainingData) =>
        new AwaitingDatum(newLexer).apply(remainingData)
      case JsonTokenGenerator.Token(TokenCloseBracket(), newLexer, remainingData) =>
        EndOfList(newLexer, remainingData)
      case JsonTokenGenerator.Token(token, _, remainingData) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token, "comma or end of list"))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingCommaOrEndOfList(newLexer))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(TokenCloseBracket(), endPos) =>
        FinalEndOfList(endPos)
      case JsonTokenGenerator.FinalToken(TokenComma(), endPos) =>
        UnexpectedEndOfInput(None, endPos)
      case JsonTokenGenerator.FinalToken(_, _) =>
        new AwaitingDatum(lexer).endOfInput()
      case JsonTokenGenerator.EndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
    }
  }

  case class AwaitingDatum(valueProducer: JValueProducer) extends JArrayProducer {
    def this(lexer: JsonTokenGenerator) = this(JValueProducer.newProducerFromLexer(lexer))

    def apply(data: WrappedCharArray): Result = valueProducer(data) match {
      case JValueProducer.More(newState) => More(new AwaitingDatum(newState))
      case JValueProducer.Value(jvalue, newLexer, remainingData) =>
        Element(jvalue, new AwaitingCommaOrEndOfList(newLexer), remainingData)
      case JValueProducer.LexerError(err) => LexerError(err)
      case JValueProducer.ParserError(err) => ParserError(err)
    }

    def endOfInput() = valueProducer.endOfInput() match {
      case JValueProducer.FinalValue(value, pos) =>
        UnexpectedEndOfInput(Some(value), pos)
      case JValueProducer.UnexpectedEndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
      case JValueProducer.LexerError(e) =>
        LexerError(e)
      case JValueProducer.ParserError(e) =>
        ParserError(e)
    }
  }
}
