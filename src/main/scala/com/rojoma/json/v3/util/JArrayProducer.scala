package com.rojoma.json.v3
package util

import io._
import ast.JValue

import JArrayProducer._
import JArrayProducerImpl._

// Incremental parsing of JSON lists
abstract class JArrayProducer private [util] (val fieldCache: FieldCache, dummy: Int) {
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

  class Builder private (lexer: JsonTokenGenerator, afterStartOfList: Boolean, fieldCache: FieldCache) {
    def this() = this(null, false, null)

    private def copy(
      lexer: JsonTokenGenerator = this.lexer,
      afterStartOfList: Boolean = this.afterStartOfList,
      fieldCache: FieldCache = this.fieldCache
    ): Builder =
      new Builder(lexer, afterStartOfList, fieldCache)

    def withLexer(lexer: JsonTokenGenerator) = copy(lexer = lexer)
    def afterStartOfList = copy(afterStartOfList = true)
    def withFieldCache(fieldCache: FieldCache) = copy(fieldCache = fieldCache)

    def build = {
      val trueLexer = if(lexer == null) JsonTokenGenerator.newGenerator else lexer
      val trueFieldCache = if(fieldCache == null) IdentityFieldCache else fieldCache

      if(afterStartOfList) new AwaitingDatumOrEndOfList(trueLexer, trueFieldCache)
      else new AwaitingStartOfList(trueLexer, trueFieldCache)
    }
  }

  def throwError(e: AnyError): Nothing = e match {
    case LexerError(e) => JsonTokenGenerator.throwError(e)
    case ParserError(e) => JsonEventGenerator.throwError(e)
    case UnexpectedEndOfInput(_, pos) => throw new JsonParserEOF(pos)
  }
}

private[util] object JArrayProducerImpl {
  class AwaitingStartOfList(lexer: JsonTokenGenerator, fc: FieldCache) extends JArrayProducer(fc, 0) {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenOpenBracket(), newLexer, remainingData) =>
        val newState = new AwaitingDatumOrEndOfList(newLexer, fieldCache)
        newState(remainingData)
      case JsonTokenGenerator.Token(token, _, _) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token, "start of list"))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingStartOfList(newLexer, fieldCache))
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

  class AwaitingDatumOrEndOfList(lexer: JsonTokenGenerator, fc: FieldCache) extends JArrayProducer(fc, 0) {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenCloseBracket(), newLexer, remainingData) =>
        EndOfList(newLexer, remainingData)
      case JsonTokenGenerator.Token(_, _, _) =>
        new AwaitingDatum(lexer, fieldCache).apply(data)
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingDatumOrEndOfList(newLexer, fieldCache))
      case e: JsonTokenGenerator.Error =>
        LexerError(e)
    }

    def endOfInput() = lexer.endOfInput() match {
      case e: JsonTokenGenerator.EndError =>
        LexerError(e)
      case JsonTokenGenerator.FinalToken(TokenCloseBracket(), pos) =>
        FinalEndOfList(pos)
      case JsonTokenGenerator.FinalToken(_, _) =>
        new AwaitingDatum(lexer, fieldCache).endOfInput()
      case JsonTokenGenerator.EndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
    }
  }

  class AwaitingCommaOrEndOfList(lexer: JsonTokenGenerator, fc: FieldCache) extends JArrayProducer(fc, 0) {
    def apply(data: WrappedCharArray): Result = lexer(data) match {
      case JsonTokenGenerator.Token(TokenComma(), newLexer, remainingData) =>
        new AwaitingDatum(newLexer, fieldCache).apply(remainingData)
      case JsonTokenGenerator.Token(TokenCloseBracket(), newLexer, remainingData) =>
        EndOfList(newLexer, remainingData)
      case JsonTokenGenerator.Token(token, _, remainingData) =>
        ParserError(JsonEventGenerator.UnexpectedToken(token, "comma or end of list"))
      case JsonTokenGenerator.More(newLexer) =>
        More(new AwaitingCommaOrEndOfList(newLexer, fieldCache))
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
        new AwaitingDatum(lexer, fieldCache).endOfInput()
      case JsonTokenGenerator.EndOfInput(pos) =>
        UnexpectedEndOfInput(None, pos)
    }
  }

  class AwaitingDatum(valueProducer: JValueProducer, fc: FieldCache) extends JArrayProducer(fc, 0) {
    def this(lexer: JsonTokenGenerator, fieldCache: FieldCache) =
      this(new JValueProducer.Builder().withLexer(lexer).withFieldCache(fieldCache).build, fieldCache)

    def apply(data: WrappedCharArray): Result = valueProducer(data) match {
      case JValueProducer.More(newState) => More(new AwaitingDatum(newState, fieldCache))
      case JValueProducer.Value(jvalue, newLexer, remainingData) =>
        Element(jvalue, new AwaitingCommaOrEndOfList(newLexer, fieldCache), remainingData)
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
