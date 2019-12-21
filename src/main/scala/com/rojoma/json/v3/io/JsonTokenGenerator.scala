package com.rojoma.json.v3
package io

import util.{WrappedCharArray, WrappedCharArrayIterator}

import scala.annotation.switch
import scala.util.control.ControlThrowable

import JsonTokenGenerator._
import JsonTokenGeneratorImpl._

sealed abstract class JsonTokenGenerator {
  def apply(chunk: WrappedCharArray): Result
  def apply(chunk: String): Result = apply(WrappedCharArray(chunk.toCharArray))
  def apply(chunk: Array[Char], offset: Int, length: Int): Result = apply(WrappedCharArray(chunk, offset, length))
  def apply(chunk: Array[Char]): Result = apply(WrappedCharArray(chunk))
  def endOfInput(): EndResult

  def lex(chunk: WrappedCharArray): SuccessfulResult = check(apply(chunk))
  def lex(chunk: String): SuccessfulResult = check(apply(chunk))
  def lex(chunk: Array[Char], offset: Int, length: Int): SuccessfulResult = check(apply(chunk, offset, length))
  def lex(chunk: Array[Char]): SuccessfulResult = check(apply(chunk))
  def finish(): SuccessfulEndResult = endOfInput() match {
    case r: SuccessfulEndResult => r
    case error: EndError => throwError(error)
  }

  private def check(result: Result) = result match {
    case r: SuccessfulResult => r
    case error: Error => throwError(error)
  }
}

object JsonTokenGenerator {
  sealed trait AnyError

  sealed abstract class Result
  sealed abstract class SuccessfulResult extends Result
  sealed abstract class Error extends Result with AnyError

  sealed trait EndResult
  sealed trait SuccessfulEndResult extends EndResult
  sealed trait EndError extends EndResult with AnyError

  case class Token(token: JsonToken, newState: JsonTokenGenerator, remainingInput: WrappedCharArray) extends SuccessfulResult
  case class More(newState: JsonTokenGenerator) extends SuccessfulResult

  case class EndOfInput(position: Position) extends SuccessfulEndResult
  case class FinalToken(token: JsonToken, endPosition: Position) extends SuccessfulEndResult

  case class UnexpectedCharacter(character: Char, expected: String, position: Position) extends Error with EndError
  case class NumberOutOfRange(number: String, position: Position) extends Error with EndError
  case class UnexpectedEndOfInput(processing: String, position: Position) extends EndError

  val newGenerator: JsonTokenGenerator = new WaitingForToken(Position(1, 1))
  def newPositionedGenerator(position: Position): JsonTokenGenerator = new WaitingForToken(position)

  def throwError(error: AnyError): Nothing = error match {
    case UnexpectedCharacter(c, e, pos) => throw new JsonUnexpectedCharacter(c,e,pos)
    case NumberOutOfRange(n,pos) => throw new JsonNumberOutOfRange(n,pos)
    case UnexpectedEndOfInput(_, pos) => throw new JsonLexerEOF(pos)
  }
}

private[io] object JsonTokenGeneratorImpl {
  class PositionedCharExtractor(underlying: WrappedCharArrayIterator, var nextCharRow: Int, var nextCharCol: Int) {
    def this(underlying: WrappedCharArrayIterator, pos: Position) = this(underlying, pos.row, pos.column)
    def atEnd = !underlying.hasNext

    def next() = {
      val result = underlying.next()
      if(result == '\n') { nextCharRow += 1; nextCharCol = 1 }
      else nextCharCol += 1
      result
    }

    def peek() = underlying.head

    def has(n: Int) = underlying.remaining >= n

    def freeze = underlying.freeze
  }

  def token(token: JsonToken, input: PositionedCharExtractor) =
    Token(token, new WaitingForToken(Position(input.nextCharRow, input.nextCharCol)), input.freeze)

  class WaitingForToken(startPos: Position) extends JsonTokenGenerator {
    def apply(chunk: WrappedCharArray): Result = {
      val input = new PositionedCharExtractor(chunk.iterator, startPos)

      val skippingWhitespace = WhitespaceSkipper.skipWhitespace(input)
      if(skippingWhitespace != null) return skippingWhitespace

      // ok, we have something that is not whitespace -- probably the (start of) a token!
      readToken(input)
    }

    def endOfInput() =
      EndOfInput(startPos)
  }

  class SkippingWhitespace(state: WhitespaceSkipper.State, pos: Position) extends JsonTokenGenerator {
    def apply(chunk: WrappedCharArray): Result = {
      val input = new PositionedCharExtractor(chunk.iterator, pos)
      val result = WhitespaceSkipper.continueSkippingWhitespace(state, input)
      if(result == null) readToken(input)
      else result
    }

    def endOfInput() =
      WhitespaceSkipper.eofInWhitespace(state, pos)
  }

  class WantingSecondCommentCharacter(slashPos: Position) extends JsonTokenGenerator {
    def position = Position(slashPos.row, slashPos.column + 1)

    def apply(chunk: WrappedCharArray): Result = {
      if(chunk.isEmpty) return More(this)

      val input = new PositionedCharExtractor(chunk.iterator, position)
      val newState = try {
        WhitespaceSkipper.readSecondCommentCharacter(input, slashPos)
      } catch {
        case UnexpectedCharacterException(c, expected, position) =>
          return UnexpectedCharacter(c, expected, position)
      }
      val result = WhitespaceSkipper.continueSkippingWhitespace(newState, input)
      if(result == null) readToken(input)
      else result
    }

    def endOfInput() =
      WhitespaceSkipper.eofWhileWantingSecondCommentCharacter(slashPos, position)
  }

  def readToken(input: PositionedCharExtractor): Result =
    (input.peek(): @switch) match {
      case '{' | '}' | '[' | ']' | ':' | ',' =>
        readSingleCharToken(input)
      case '"' | '\'' =>
        StringReader.readString(input)
      case '-' =>
        NumberReader.readNumber(input)
      case c =>
        if(NumberReader.isDigit(c)) NumberReader.readNumber(input)
        else if(Character.isUnicodeIdentifierStart(c)) IdentifierReader.readIdentifier(input)
        else UnexpectedCharacter(c, "start of datum", Position(input.nextCharRow, input.nextCharCol))
    }

  def readSingleCharToken(input: PositionedCharExtractor) = {
    val position = Position(input.nextCharRow, input.nextCharCol)
    val t = (input.next(): @switch) match {
      case '{' => TokenOpenBrace()(position)
      case '}' => TokenCloseBrace()(position)
      case '[' => TokenOpenBracket()(position)
      case ']' => TokenCloseBracket()(position)
      case ':' => TokenColon()(position)
      case ',' => TokenComma()(position)
    }
    token(t, input)
  }

  class ReadingString(state: StringReader.CompoundState, chunks: List[String], boundary: Char, stringStartPos: Position, pos: Position) extends JsonTokenGenerator {
    def apply(chunk: WrappedCharArray): Result =
      StringReader.continueReadingString(state, chunks, boundary, stringStartPos, new PositionedCharExtractor(chunk.iterator, pos))

    def endOfInput() =
      UnexpectedEndOfInput("string", pos)
  }

  class ReadingIdentifier(chunks: List[String], startPos: Position, pos: Position) extends JsonTokenGenerator {
    def apply(chunk: WrappedCharArray): Result = {
      val input = new PositionedCharExtractor(chunk.iterator, pos)
      IdentifierReader.continueReadingIdentifier(chunks, startPos, input)
    }

    def endOfInput(): EndResult = {
      val identToken = TokenIdentifier(chunks.reverse.mkString)(startPos)
      FinalToken(identToken, pos)
    }
  }

  class ReadingNumber(state: Int, chunks: List[String], startPos: Position, pos: Position) extends JsonTokenGenerator {
    def apply(chunk: WrappedCharArray): Result = {
      val input = new PositionedCharExtractor(chunk.iterator, pos)
      NumberReader.continueReadingNumber(state, chunks, startPos, input)
    }

    def endOfInput(): EndResult = {
      NumberReader.eofInNumber(state, chunks, startPos, pos)
    }
  }

  case class UnexpectedCharacterException(c: Char, expected: String, position: Position) extends ControlThrowable

  class ReaderBase {
    def isDigit(c: Char) = c >= '0' && c <= '9'

    def addChunk(sb: StringBuilder, chunksSoFar: List[String]): List[String] =
      if(sb.length == 0) chunksSoFar
      else sb.toString :: chunksSoFar

    def mergeChunks(sb: StringBuilder, chunks: List[String]): String =
      if(chunks eq Nil) sb.toString
      else addChunk(sb, chunks).reverse.mkString
  }

  object WhitespaceSkipper extends ReaderBase {
    final type State = Int
    final val ReadingOrdinaryWhitespace = 0
    final val ReadingToEOL = 1
    final val LookingFor_* = 2
    final val LookingFor_/ = 3
    final val Done = 4

    def skipWhitespace(input: PositionedCharExtractor): Result = {
      // this will produce a More if it encounters the end of input,
      // so the caller does not need to check for that afterward.
      continueSkippingWhitespace(ReadingOrdinaryWhitespace, input)
    }

    def eofInWhitespace(state: State, pos: Position): EndResult = {
      (state: Int @switch) match {
        case ReadingOrdinaryWhitespace | ReadingToEOL | Done =>
          EndOfInput(pos)
        case LookingFor_* | LookingFor_/ =>
          UnexpectedEndOfInput("comment", pos)
      }
    }

    def eofWhileWantingSecondCommentCharacter(slashPos: Position, pos: Position): EndResult = {
      // hrmrmrmrm.  Do I report the error *here* or where the slash was?  Or to put it another way:
      // is this a stray slash or an interrupted comment?
      UnexpectedCharacter('/', "datum", slashPos)
    }

    case class EndOfInputPreparingToReadSecondCommentCharacter(position: Position) extends ControlThrowable

    def continueSkippingWhitespace(initialState: State, input: PositionedCharExtractor): Result = {
      try {
        var state = initialState
        while(!input.atEnd) {
          state = (state: Int @switch) match {
            case ReadingOrdinaryWhitespace => readOrdinaryWhitespace(input)
            case ReadingToEOL => readToEOL(input)
            case LookingFor_* => read_*(input)
            case LookingFor_/ => read_/(input)
            case Done => return null
          }
        }

        if(state == ReadingOrdinaryWhitespace) return More(new WaitingForToken(Position(input.nextCharRow, input.nextCharCol)))
        else More(new SkippingWhitespace(state, Position(input.nextCharRow, input.nextCharCol)))
      } catch {
        case EndOfInputPreparingToReadSecondCommentCharacter(pos) =>
          More(new WantingSecondCommentCharacter(pos))
        case UnexpectedCharacterException(c, expected, pos) =>
          UnexpectedCharacter(c, expected, pos)
      }
    }

    def readOrdinaryWhitespace(input: PositionedCharExtractor): State = {
      do {
        val c = input.peek()
        if(Character.isWhitespace(c)) input.next()
        else if(c == '/') {
          val slashPos = Position(input.nextCharRow, input.nextCharCol)
          input.next()
          if(input.atEnd) throw EndOfInputPreparingToReadSecondCommentCharacter(slashPos) // ick, but it should be super rare!
          else return readSecondCommentCharacter(input, slashPos)
        } else return Done
      } while(!input.atEnd)
      ReadingOrdinaryWhitespace
    }

    def readSecondCommentCharacter(input: PositionedCharExtractor, slashPos: Position): State = {
      input.next() match {
        case '*' =>
          if(input.atEnd) LookingFor_*
          else read_*(input)
        case '/' =>
          if(input.atEnd) ReadingToEOL
          else readToEOL(input)
        case _ =>
          throw UnexpectedCharacterException('/', "start of datum", slashPos)
      }
    }

    def readToEOL(input: PositionedCharExtractor): State = {
      do {
        val c = input.next()
        if(c == '\n') return ReadingOrdinaryWhitespace
      } while(!input.atEnd)
      ReadingToEOL
    }

    def read_*(input: PositionedCharExtractor): State = {
      do {
        val c = input.next()
        if(c == '*') {
          if(input.atEnd) return LookingFor_/
          else if(input.next() == '/') return ReadingOrdinaryWhitespace
          // otherwise wasn't end-of-comment so just keep going
        }
      } while(!input.atEnd)
      LookingFor_*
    }

    def read_/(input: PositionedCharExtractor): State = {
      if(input.next() == '/') ReadingOrdinaryWhitespace
      else if(input.atEnd) LookingFor_/
      else read_*(input)
    }
  }

  object StringReader extends ReaderBase {
    // States for string-reading
    final type State = Int
    final val ReadingOrdinaryCharacter = 0
    final val ReadingEscape = 1
    final val ReadingUnicode0 = 2
    final val ReadingUnicode1 = 3
    final val ReadingUnicode2 = 4
    final val ReadingUnicode3 = 5

    // A "CompoundState" is [16 bits high surrogate] [12 bits pending unicode] [1 bit not-expecting-low-surrogate] [3 bits actual state]
    // Which means there's room for two more States!  What extravagance!
    type CompoundState = Int
    @inline def startState: CompoundState = ReadingOrdinaryCharacter | 0x00000008
    @inline def extractState(state: CompoundState): State = state & 7
    @inline def updateState(state: CompoundState, newState: State) = (state & ~7) | newState
    @inline def extractPendingUnicode(state: CompoundState): Int = (state >> 4) & 0xfff
    @inline def updatePendingUnicode(state: CompoundState, newPendingUnicode: Int) = (state & ~(0xfff << 4)) | (newPendingUnicode << 4)
    @inline def expectingLowSurrogate(state: CompoundState): Boolean = (state & 0x00000008) == 0
    @inline def extractHighSurrogate(state: CompoundState): Int = state >>> 16
    // updateHighSurrogate is the reason the flag bit is "NOT expecting low surrogate"; if it were the
    // other way around it'd have to clear the four high bits and set the flag which would take TWO
    // operations instead of one.  Since none of the other ops really care, this wins.
    @inline def updateHighSurrogate(state: CompoundState, newHighSurrogate: Char) = (state & 0x0000fff7) | (newHighSurrogate.toInt << 16)
    @inline def clearHighSurrogate(state: CompoundState) = state | 0x00000008

    final val BadChar = 0xfffd.toChar
    @inline final def isSurrogate(c: Char) = c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE
    @inline final def isHighSurrogate(c: Char) = Character.isHighSurrogate(c)
    @inline final def isLowSurrogate(c: Char) = Character.isLowSurrogate(c)

    def readString(input: PositionedCharExtractor): Result = {
      val startPos = Position(input.nextCharRow, input.nextCharCol)
      val boundary = input.next()
      continueReadingString(startState, Nil, boundary, startPos, input)
    }

    def continueReadingString(initialState: CompoundState, chunks: List[String], boundary: Char, startPos: Position, input: PositionedCharExtractor): Result = {
      try {
        val sb = new StringBuilder
        var state = initialState
        while(!input.atEnd) {
          state = (extractState(state): Int @switch) match {
            case ReadingOrdinaryCharacter =>
              if(input.peek() == boundary) {
                input.next() // pass over closing quote
                if(expectingLowSurrogate(state)) sb.append(BadChar)
                val stringToken = TokenString(mergeChunks(sb, chunks))(startPos)
                return token(stringToken, input)
              } else {
                readOrdinaryCharacters(state, sb, input, boundary)
              }
            case ReadingEscape =>
              readEscapeChar(state, sb, input)
            case ReadingUnicode0 =>
              readUnicode0(state, sb, input)
            case ReadingUnicode1 =>
              readUnicode1(state, input)
            case ReadingUnicode2 =>
              readUnicode2(state, input)
            case ReadingUnicode3 =>
              readUnicode3(state, sb, input)
          }
        }
        More(new ReadingString(state, addChunk(sb, chunks), boundary, startPos, Position(input.nextCharRow, input.nextCharCol)))
      } catch {
        case UnexpectedCharacterException(c, expected, pos) => UnexpectedCharacter(c, expected, pos)
      }
    }

    def readOrdinaryCharacters(initialState: CompoundState, sb: StringBuilder, input: PositionedCharExtractor, boundary: Char): CompoundState = {
      // precondition: there is data available and it is not the boundary-char
      var state = initialState
      do { // we'll eat as much as we can without running into a non-ordinary character
        val c = input.next()
        if(c == '\\') { // oops, here's a non-ordinary character!
          val newState = if(input.atEnd) updateState(state, ReadingEscape) else readEscapeChar(state, sb, input)
          return newState
        } else state = addChar(state, sb, c)
      } while(!input.atEnd && input.peek() != boundary)
      state
    }

    def addChar(state: CompoundState, sb: StringBuilder, c: Char): CompoundState = {
      // contract: only the state's high surrogate field is potentially changed
      if(expectingLowSurrogate(state)) {
        if(isLowSurrogate(c)) {
          // yay, correct surrogate pair
          sb.append(extractHighSurrogate(state).toChar)
          sb.append(c)
          clearHighSurrogate(state)
        } else {
          // oops, we have a pending high surrogate but "c" is not low.
          sb.append(BadChar)
          if(isHighSurrogate(c)) {
            updateHighSurrogate(state, c)
          } else {
            sb.append(c)
            clearHighSurrogate(state)
          }
        }
      } else if(isSurrogate(c)) {
        if(isHighSurrogate(c)) {
          updateHighSurrogate(state, c)
        } else { // oops, stray low surrogate
          sb.append(BadChar)
          state
        }
      } else {
        sb.append(c)
        state
      }
    }

    def readEscapeChar(state: CompoundState, sb: StringBuilder, input: PositionedCharExtractor): CompoundState = {
      val row = input.nextCharRow
      val col = input.nextCharCol
      val probableEndState = updateState(state, ReadingOrdinaryCharacter)
      (input.next(): @switch) match {
        case c@('"' | '\'' | '\\' | '/') => addChar(probableEndState, sb, c)
        case 'b' => addChar(probableEndState, sb, '\b')
        case 'f' => addChar(probableEndState, sb, '\f')
        case 'n' => addChar(probableEndState, sb, '\n')
        case 'r' => addChar(probableEndState, sb, '\r')
        case 't' => addChar(probableEndState, sb, '\t')
        case 'u' =>
          if(input.atEnd) updateState(probableEndState, ReadingUnicode0)
          else readUnicode0(probableEndState, sb, input)
        case c => throw UnexpectedCharacterException(c, "string escape character", Position(row, col))
      }
    }

    def readUnicode0(state: CompoundState, sb: StringBuilder, input: PositionedCharExtractor): CompoundState = {
      if(input.has(4)) { // common case; all four chars are available.
        val h1, h2, h3, h4 = readHexDigit(input)
        addChar(updateState(state, ReadingOrdinaryCharacter), sb, ((h1 << 12) | (h2 << 8) | (h3 << 4) | h4).toChar)
      } else { // slow path: one char at a time.
        updateState(updatePendingUnicode(state, readHexDigit(input)), ReadingUnicode1)
      }
    }

    def readUnicode1(state: CompoundState, input: PositionedCharExtractor): CompoundState = {
      updateState(updatePendingUnicode(state, (extractPendingUnicode(state) << 4) | readHexDigit(input)), ReadingUnicode2)
    }

    def readUnicode2(state: CompoundState, input: PositionedCharExtractor): CompoundState = {
      updateState(updatePendingUnicode(state, (extractPendingUnicode(state) << 4) | readHexDigit(input)), ReadingUnicode3)
    }

    def readUnicode3(state: CompoundState, sb: StringBuilder, input: PositionedCharExtractor): CompoundState = {
      val c = ((extractPendingUnicode(state) << 4) | readHexDigit(input)).toChar
      addChar(updateState(state, ReadingOrdinaryCharacter), sb, c)
    }

    def readHexDigit(input: PositionedCharExtractor): Int = {
      val row = input.nextCharRow
      val col = input.nextCharCol
      input.next() match {
        case c if isDigit(c) => c.toInt - '0'.toInt
        case c if 'a' <= c && c <= 'f' => 10 + c.toInt - 'a'.toInt
        case c if 'A' <= c && c <= 'F' => 10 + c.toInt - 'A'.toInt
        case c => throw UnexpectedCharacterException(c, "hex digit", Position(row, col))
      }
    }
  }

  object IdentifierReader extends ReaderBase {
    def readIdentifier(input: PositionedCharExtractor): Result = {
      val startPos = Position(input.nextCharRow, input.nextCharCol)
      val sb = new StringBuilder
      sb.append(input.next())
      continueReadingIdentifier2(sb, Nil, startPos, input)
    }

    def continueReadingIdentifier(chunks: List[String], startPos: Position, input: PositionedCharExtractor): Result =
      continueReadingIdentifier2(new StringBuilder, chunks, startPos, input)

    def continueReadingIdentifier2(sb: StringBuilder, chunks: List[String], startPos: Position, input: PositionedCharExtractor): Result = {
      while(!input.atEnd && Character.isUnicodeIdentifierPart(input.peek())) {
        sb.append(input.next())
      }
      if(input.atEnd) More(new ReadingIdentifier(addChunk(sb, chunks), startPos, Position(input.nextCharRow, input.nextCharCol)))
      else {
        val identifier = TokenIdentifier(mergeChunks(sb, chunks))(startPos)
        token(identifier, input)
      }
    }
  }

  object NumberReader extends ReaderBase {
    final type State = Int
    final val ReadingSign = 0
    final val ReadingFirstWholePartDigit = 1
    final val ReadingWholePart = 2
    final val ReadingFirstFracPartDigit = 3
    final val ReadingFracPart = 4
    final val ReadingExponentSign = 5
    final val ReadingFirstExponentDigit = 6
    final val ReadingExponent = 7
    final val Done = 8

    case class NumberOutOfRangeException(number: String, position: Position) extends ControlThrowable

    def readNumber(input: PositionedCharExtractor): Result =
      continueReadingNumber(ReadingSign, Nil, Position(input.nextCharRow, input.nextCharCol), input)

    def eofInNumber(state: State, chunks: List[String], startPos: Position, pos: Position): EndResult = {
      (state: Int @switch) match {
        case ReadingSign | ReadingFirstWholePartDigit | ReadingFirstFracPartDigit | ReadingExponentSign | ReadingFirstExponentDigit =>
          UnexpectedEndOfInput("number", pos)
        case _ =>
          FinalToken(toNumberToken(chunks.reverse.mkString, startPos),
                     pos)
      }
    }

    def continueReadingNumber(initialState: State, chunks: List[String], startPos: Position, input: PositionedCharExtractor): Result = {
      try {
        var state = initialState
        val sb = new StringBuilder

        while(!input.atEnd) {
          state = (state: Int @switch) match {
            case ReadingSign => readSign(sb, input)
            case ReadingFirstWholePartDigit => readWholePart(sb, input, true)
            case ReadingWholePart => readWholePart(sb, input, false)
            case ReadingFirstFracPartDigit => readFracPart(sb, input, true)
            case ReadingFracPart => readFracPart(sb, input, false)
            case ReadingExponentSign => readExponentSign(sb, input)
            case ReadingFirstExponentDigit => readExponent(sb, input, true)
            case ReadingExponent => readExponent(sb, input, false)
            case Done =>
              val number = mergeChunks(sb, chunks)
              try { return token(toNumberToken(number, startPos), input) }
              catch { case NumberOutOfRangeException(number, pos) => return NumberOutOfRange(number, pos) }
          }
        }
        More(new ReadingNumber(state, addChunk(sb, chunks), startPos, Position(input.nextCharRow, input.nextCharCol)))
      } catch {
        case UnexpectedCharacterException(c, expected, pos) => UnexpectedCharacter(c, expected, pos)
      }
    }

    def toNumberToken(text: String, startPos: Position) = {
      if(!ReaderUtils.isBigDecimalizable(text)) throw NumberOutOfRangeException(text, startPos)
      TokenNumber(text)(startPos)
    }

    def readSign(sb: StringBuilder, input: PositionedCharExtractor): State = {
      if(input.peek() == '-') sb.append(input.next())
      if(input.atEnd) ReadingFirstWholePartDigit
      else readWholePart(sb, input, true)
    }

    def readWholePart(sb: StringBuilder, input: PositionedCharExtractor, firstDigit: Boolean): State = {
      readDigits(sb, input, firstDigit)
      if(input.atEnd) ReadingWholePart
      else {
        (input.peek(): @switch) match {
          case '.' =>
            sb.append(input.next())
            if(input.atEnd) ReadingFirstFracPartDigit
            else readFracPart(sb, input, true)
          case 'e' | 'E' =>
            readE(sb, input)
          case _ =>
            Done
        }
      }
    }

    def readDigits(sb: StringBuilder, input: PositionedCharExtractor, atLeastOne: Boolean) {
      if(atLeastOne) readDigit(sb, input)
      while(!input.atEnd && isDigit(input.peek())) {
        readDigit(sb, input)
      }
    }

    def readDigit(sb: StringBuilder, input: PositionedCharExtractor) {
      val row = input.nextCharRow
      val col = input.nextCharRow
      val c = input.next()
      if(isDigit(c)) sb.append(c)
      else throw UnexpectedCharacterException(c, "digit", Position(row, col))
    }

    def readFracPart(sb: StringBuilder, input: PositionedCharExtractor, firstDigit: Boolean): State = {
      readDigits(sb, input, firstDigit)
      if(input.atEnd) ReadingFracPart
      else {
        (input.peek(): @switch) match {
          case 'e' | 'E' =>
            readE(sb, input)
          case _ =>
            Done
        }
      }
    }

    def readE(sb: StringBuilder, input: PositionedCharExtractor): State = {
      sb.append(input.next())
      if(input.atEnd) ReadingExponentSign
      else readExponentSign(sb, input)
    }

    def readExponentSign(sb: StringBuilder, input: PositionedCharExtractor): State = {
      if(input.peek() == '+' || input.peek() == '-') sb.append(input.next())
      else sb.append('+') // ensure there's always a sign
      if(input.atEnd) ReadingFirstExponentDigit
      else readExponent(sb, input, true)
    }

    def readExponent(sb: StringBuilder, input: PositionedCharExtractor, firstDigit: Boolean): State = {
      readDigits(sb, input, firstDigit)
      if(input.atEnd) ReadingExponent
      else Done
    }
  }
}
