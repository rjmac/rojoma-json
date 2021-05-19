package com.rojoma.json.v3
package io

import java.io.Reader

import `-impl`.util.AbstractBufferedIterator

/** Turns a raw character-stream into an event stream, checking for JSON
 * well-formedness.
 *
 * A `FusedBlockJsonEventIterator` checks a character stream for syntactic correctness
 * and produces events that reflect the syntax of JSON.
 *
 * As extension, this class allows comments and for unquoted identifiers to be used
 * as object keys.
 *
 * @see [[com.rojoma.json.v3.io.JsonEventIterator]]
 * @see [[com.rojoma.json.v3.io.JsonEventGenerator]]
 * @see [[com.rojoma.json.v3.io.JsonEvent]]
 */
class FusedBlockJsonEventIterator(input: Reader, fieldCache: FieldCache = IdentityFieldCache, blockSize: Int = 1024) extends AbstractBufferedIterator[JsonEvent] {
  def this(text: String) = this(new java.io.StringReader(text))
  def this(text: String, fieldCache: FieldCache) = this(new java.io.StringReader(text), fieldCache)

  private [this] val block = new Array[Char](blockSize)
  private [this] var pos = 0
  private [this] var end = 0

  private [this] var atTop = true // this is the value BEFORE "available" was last set
  private [this] var available: JsonEvent = null

  private [this] var stack = new Array[Boolean](16) // values are true for "parsing array" and false for "parsing object"
  private [this] var stackPtr = -1

  private [this] val scratch = new StringBuilder

  // prevent toString from having side-effects
  override def toString() = {
    if((available ne null) || (pos != end)) "non-empty iterator"
    else "possibly-empty iterator"
  }

  private def push(intoArray: Boolean): Unit = {
    def growStack(): Unit = {
      val newStack = new Array[Boolean](stack.length * 2)
      System.arraycopy(stack, 0, newStack, 0, stack.length)
      stack = newStack
    }
    stackPtr += 1
    if(stackPtr == stack.length) growStack()
    stack(stackPtr) = intoArray
  }

  private def pop(): Unit = { assert(stackPtr >= 0); stackPtr -= 1 }

  // Meaningful only if the stack is not empty and the meaning
  // varies depending on the kind on the top of the stack:
  //  * object (top == false):
  //     0 => awaiting field name or }
  //     1 => awaiting field name preceded by comma, or }
  //     2 => awaiting datum
  //  * array (top == true):
  //     0 => awaiting datum or ]
  //     1 => awating datum preceded by comma, or ]
  // Note that the new value after finishing a datum is
  // always the same: it's always 1.
  private [this] var compoundReadState: Int = _

  private [this] var nextCharRow = 1 // This is the position of the next char returned from "nextChar()" or "peekChar()"
  private [this] var nextCharCol = 1

  private def lexerError(receivedChar: Char, expected: String, row: Int, col: Int): Nothing = {
    throw new JsonUnexpectedCharacter(receivedChar, expected, Position(row, col))
  }

  private def refill(): Boolean =
    input.read(block) match {
      case -1 =>
        false
      case n =>
        pos = 0
        end = n
        true
    }

  private def throwEOF() =
    throw new JsonLexerEOF(Position(nextCharRow, nextCharCol))

  private def atEOF(): Boolean =
    pos == end && !refill()

  private def skipCharNotAtEOF(): Unit = {
    if(block(pos) == '\n') { nextCharRow += 1; nextCharCol = 1 }
    else { nextCharCol += 1 }
    pos += 1
  }

  private def peekChar() = {
    if(atEOF()) throwEOF()
    block(pos)
  }

  private def peekCharNotAtEOF() = block(pos)

  private def nextChar() = {
    val result = peekChar()
    skipCharNotAtEOF()
    result
  }

  private def nextCharNotAtEOF() = {
    val result = block(pos)
    skipCharNotAtEOF()
    result
  }

  private def skipToEndOfLine() = while(!atEOF() && peekCharNotAtEOF() != '\n') skipCharNotAtEOF()

  private def skipBlockComment(): Unit = {
    var last = nextChar()
    while(last != '*' || peekChar() != '/') last = nextChar()
    skipCharNotAtEOF() // skip final '/'
  }

  private def skipComment(): Unit = {
    skipCharNotAtEOF() // skip opening "/"
    peekChar() match {
      case '/' => skipCharNotAtEOF(); skipToEndOfLine()
      case '*' => skipCharNotAtEOF(); skipBlockComment()
      case c => lexerError(c, "/ or *", nextCharRow, nextCharCol)
    }
  }

  @annotation.tailrec
  private def skipWhitespace(): Unit = {
    while(!atEOF() && Character.isWhitespace(peekCharNotAtEOF())) skipCharNotAtEOF()
    if(!atEOF() && peekCharNotAtEOF() == '/') { skipComment(); skipWhitespace() }
  }

  private def advance(): Unit = {
    atTop = stackPtr == -1
    skipWhitespace()
    if(!atEOF()) available = readEvent()
  }

  def hasNext: Boolean = {
    if(available == null) advance()
    available != null
  }

  def head = {
    if(!hasNext) {
      throw new NoSuchTokenException(Position(nextCharRow, nextCharCol))
    }
    available
  }

  def next(): JsonEvent = {
    val result = head
    available = null
    result
  }

  private def readEvent(): JsonEvent = {
    if(stackPtr == -1) {
      readDatumEvent("datum")
    } else if(stack(stackPtr)) {
      readArrayEvent()
    } else {
      readObjectEvent()
    }
  }

  private def readDatumEvent(expected: String): JsonEvent = {
    skipWhitespace()
    peekChar() match {
      case '{' => openObject()
      case '[' => openArray()
      case '"' | '\'' => readStringEvent()
      case '-' => readNumberEvent()
      case c =>
        if(isDigit(c)) readNumberEvent()
        else if(Character.isUnicodeIdentifierStart(c)) readIdentifierEvent()
        else badToken(expected)
    }
  }

  private def badToken(expected: String): Nothing = {
    val p = Position(nextCharRow, nextCharCol)
    val token = peekChar() match {
      case '{' => TokenOpenBrace()(p)
      case '}' => TokenCloseBrace()(p)
      case '[' => TokenOpenBracket()(p)
      case ']' => TokenCloseBracket()(p)
      case ',' => TokenComma()(p)
      case ':' => TokenColon()(p)
      case '"' | '\'' => TokenString(readString())(p)
      case c if isDigit(c) || c == '-' => TokenNumber(readNumber())(p)
      case c if Character.isUnicodeIdentifierStart(c) => TokenIdentifier(readIdentifier())(p)
      case c => lexerError(c, expected, nextCharRow, nextCharCol)
    }
    throw new JsonUnexpectedToken(token, expected)
  }

  private def readObjectEvent(): JsonEvent =
    compoundReadState match {
      case 0 => readObjectFieldName()
      case 1 => readObjectFieldNamePrecededByComma()
      case 2 => readObjectDatumPrecededByColon()
    }

  private def readObjectFieldName(): JsonEvent = {
    skipWhitespace()
    if(peekChar() == '}') {
      endObject()
    } else {
      val fieldName = readFieldNameEvent("field name or end of object")
      compoundReadState = 2
      fieldName
    }
  }

  private def readObjectFieldNamePrecededByComma(): JsonEvent = {
    skipWhitespace()
    peekChar() match {
      case ',' =>
        skipCharNotAtEOF()

        // compat with JsonEventIterator: if the EOF happens now,
        // just end the read
        skipWhitespace()
        if(atEOF()) return null

        val fieldName = readFieldNameEvent("field name")
        compoundReadState = 2
        fieldName
      case '}' =>
        endObject()
      case _ =>
        badToken("comma or end of object")
    }
  }

  private def readObjectDatumPrecededByColon(): JsonEvent = {
    skipWhitespace()
    if(peekChar() == ':') {
      skipCharNotAtEOF()

      // compat with JsonEventIterator: if the EOF happens now,
      // just end the read
      skipWhitespace()
      if(atEOF()) return null

      readDatumEvent("datum")
    } else {
      badToken("colon")
    }
  }

  private def readFieldNameEvent(expected: String): JsonEvent = {
    skipWhitespace()
    val row = nextCharRow
    val col = nextCharCol
    val field = peekChar() match {
      case '"' | '\'' => readString()
      case c if Character.isUnicodeIdentifierStart(c) => readIdentifier()
      case _ => badToken(expected)
    }
    nonDatum(FieldEvent(fieldCache(field, stackPtr))(Position(row, col)))
  }

  private def readArrayEvent(): JsonEvent =
    compoundReadState match {
      case 0 => readArrayElement()
      case 1 => readArrayElementPrecededByComma()
    }

  private def readArrayElement(): JsonEvent = {
    skipWhitespace()
    if(peekChar() == ']') {
      endArray()
    } else {
      readDatumEvent("datum or end of array")
    }
  }

  private def readArrayElementPrecededByComma(): JsonEvent = {
    skipWhitespace()
    peekChar() match {
      case ',' =>
        skipCharNotAtEOF()

        // compat with JsonEventIterator: if the EOF happens now,
        // just end the read
        skipWhitespace()
        if(atEOF()) return null

        readDatumEvent("datum")
      case ']' =>
        endArray()
      case _ =>
        badToken("comma or end of array")
    }
  }

  private def openObject(): JsonEvent = {
    // Precondition: positioned on '{'
    val row = nextCharRow
    val col = nextCharCol
    skipCharNotAtEOF()
    push(false)
    compoundReadState = 0
    nonDatum(StartOfObjectEvent()(Position(row, col)))
  }

  private def endObject(): JsonEvent = {
    // Precondition: positioned on '}'
    val row = nextCharRow
    val col = nextCharCol
    skipCharNotAtEOF()
    pop()
    finishDatum(EndOfObjectEvent()(Position(row, col)))
  }

  private def openArray(): JsonEvent = {
    // Precondition: positioned on '['
    val row = nextCharRow
    val col = nextCharCol
    skipCharNotAtEOF()
    push(true)
    compoundReadState = 0
    nonDatum(StartOfArrayEvent()(Position(row, col)))
  }

  private def endArray(): JsonEvent = {
    // Precondition: positioned on ']'
    val row = nextCharRow
    val col = nextCharCol
    skipCharNotAtEOF()
    pop()
    finishDatum(EndOfArrayEvent()(Position(row, col)))
  }

  private def readStringEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(StringEvent(readString())(Position(row, col)))
  }

  private def readString(): String = {
    scratch.setLength(0)
    val Boundary = nextChar()
    while(peekChar() != Boundary) {
      readPotentialSurrogatePair(readChar(), Boundary)
    }
    skipCharNotAtEOF() // skip closing quote
    scratch.toString
  }

  private def readPotentialSurrogatePair(c: Char, endOfString: Char): Unit = {
    if(c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE) {
      readSurrogatePair(c, endOfString)
    } else {
      scratch += c
    }
  }

  private def badChar = 0xfffd.toChar

  @annotation.tailrec
  private def readSurrogatePair(c: Char, endOfString: Char): Unit = {
    if(Character.isHighSurrogate(c)) {
      if(peekChar() == endOfString) {
        scratch += badChar
      } else {
        val potentialSecondHalf = readChar()
        if(Character.isLowSurrogate(potentialSecondHalf)) {
          scratch += c
          scratch += potentialSecondHalf
        } else {
          scratch += badChar
          if(potentialSecondHalf >= Character.MIN_SURROGATE && potentialSecondHalf <= Character.MAX_SURROGATE) {
            readSurrogatePair(potentialSecondHalf, endOfString)
          } else {
            scratch += potentialSecondHalf
          }
        }
      }
    } else {
      scratch += badChar
    }
  }

  private def readChar(): Char = {
    nextChar() match {
      case '\\' => readEscapedCharacter()
      case c => c
    }
  }

  private def readEscapedCharacter(): Char = {
    def ret(c: Char) = { skipCharNotAtEOF(); c }
    peekChar() match {
      case '"' => ret('"')
      case '\'' => ret('\'')
      case '\\' => ret('\\')
      case '/' => ret('/')
      case 'b' => ret('\b')
      case 'f' => ret('\f')
      case 'n' => ret('\n')
      case 'r' => ret('\r')
      case 't' => ret('\t')
      case 'u' => skipCharNotAtEOF(); readUnicodeCharacter()
      case c => lexerError(c, "string escape character", nextCharRow, nextCharCol)
    }
  }

  private def readUnicodeCharacter(): Char = {
    val h1, h2, h3, h4 = readHexDigit()
    ((h1 << 12) | (h2 << 8) | (h3 << 4) | h4).toChar
  }

  private def isDigit(c: Char) = '0' <= c && c <= '9'

  private def readHexDigit(): Int = {
    peekChar() match {
      case c if isDigit(c) =>
        skipCharNotAtEOF()
        c.toInt - '0'.toInt
      case c if 'a' <= c && c <= 'f' =>
        skipCharNotAtEOF()
        10 + c.toInt - 'a'.toInt
      case c if 'A' <= c && c <= 'F' =>
        skipCharNotAtEOF()
        10 + c.toInt - 'A'.toInt
      case c =>
        lexerError(c, "hex digit", nextCharRow, nextCharCol)
    }
  }

  private def readIdentifierEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(IdentifierEvent(readIdentifier())(Position(row, col)))
  }

  private def readIdentifier(): String = {
    scratch.setLength(0)
    scratch += nextChar()
    while(!atEOF() && Character.isUnicodeIdentifierPart(peekCharNotAtEOF())) scratch += nextCharNotAtEOF()
    scratch.toString()
  }

  private def readDigit(): Char = {
    if(!isDigit(peekChar())) lexerError(peekChar(), "digit", nextCharRow, nextCharCol)
    nextChar()
  }

  private def readNumberEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(NumberEvent(readNumber())(Position(row, col)))
  }

  private def readNumber(): String = {
    // JSON numbers match (a subset of) the language generated by
    // the regular expression:
    //    -?\d+(\.\d+)?([eE][+-]?\d+)?
    // We'll match the whole thing, within the limits of BigDecimal
    scratch.setLength(0)

    val startPos = Position(nextCharRow, nextCharCol)

    if(peekChar() == '-') scratch += nextChar()

    while { scratch += readDigit(); !atEOF() && isDigit(peekCharNotAtEOF()) } do ()

    val hasFrac = !atEOF() && peekCharNotAtEOF() == '.'
    if(hasFrac) {
      scratch += nextChar() // skip decimal
      while { scratch += readDigit(); !atEOF() && isDigit(peekCharNotAtEOF()) } do ()
    }

    val hasExponent = !atEOF() && (peekCharNotAtEOF() == 'e' || peekCharNotAtEOF() == 'E')
    if(hasExponent) {
      scratch += nextCharNotAtEOF() // skip e/E

      if(peekChar() == '-' || peekCharNotAtEOF() == '+') scratch += nextCharNotAtEOF()
      else scratch += '+' // ensure there's always a sign

      val exponentDigitsStart = scratch.length
      while { scratch += readDigit(); !atEOF() && isDigit(peekCharNotAtEOF()) } do ()

      // this relies on the exponent being the last thing read
      val result = scratch.toString
      if(!ReaderUtils.isBigDecimalizableUnsignedExponent(result, exponentDigitsStart)) {
        throw new JsonNumberOutOfRange(result, startPos)
      }
      result
    } else {
      scratch.toString
    }
  }

  private def finishDatum(event: JsonEvent): JsonEvent = {
    compoundReadState = 1
    event
  }

  private def nonDatum(event: JsonEvent): JsonEvent = {
    event
  }

  /**
   * Finish reading the "current" object or array, where "current" is
   * defined as "the most recent compound object started by `next()`.
   * If a top-level object has not been started, this does nothing.
   *
   * @return This iterator
   * @throws JsonEOF If the end-of-input occurs before finishing
   *   this object.
   */
  def skipRestOfCompound(): this.type = {
    hasNext // hasNext to make sure atTop is in an accurate state
    if(!atTop) {
      try {
        var count = 0
        while {
          val ev = next()
          ev match {
            case StartOfObjectEvent() | StartOfArrayEvent() => count += 1
            case EndOfObjectEvent() | EndOfArrayEvent() => count -= 1
            case _ => /* nothing */
          }

          count >= 0
        } do ()
      } catch {
        case e: NoSuchTokenException => throw new JsonParserEOF(e.position)
        case _: NoSuchElementException => throw new JsonParserEOF(Position(-1, -1))
      }
    }
    this
  }

  @inline
  final def dropRestOfCompound() = skipRestOfCompound()

  /** Skips the next datum that would be returned entirely.  If the next event
   * is the start of a array or object, `skipRestOfCompound()` is called to
   * pass over it. If it's a field event, the field and its associated value
   * are skipped. If it's the end of a array or object, no position change is
   * made and the next call to `head` or `next()` will still return the end
   * event.  Otherwise, it's an atom and is consumed.
   *
   * @return This iterator
   * @throws NoSuchElementException if this iterator is empty at the start of the call
   * @throws JsonEOF if the token iterator runs out before the end of the datum
   */
  def skipNextDatum(): this.type = head match {
    case StartOfObjectEvent() | StartOfArrayEvent() =>
      next()
      skipRestOfCompound()
    case FieldEvent(_) =>
      next()
      skipNextDatum()
    case EndOfObjectEvent() | EndOfArrayEvent() =>
      this
    case _ =>
      next()
      this
  }

  @inline
  final def dropNextDatum() = skipNextDatum()
}
