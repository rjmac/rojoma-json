package com.rojoma.json
package io

import java.io.Reader

/** Turns a raw character-stream into an event stream, checking for JSON
 * well-formedness.
 *
 * A `FusedBlockJsonEventIterator` checks a character stream for syntactic correctness
 * and produces events that reflect the syntax of JSON.
 *
 * As extension, this class allows comments and for unquoted identifiers to be used
 * as object keys.
 *
 * @see [[com.rojoma.json.io.JsonEventIterator]]
 * @see [[com.rojoma.json.io.JsonEventGenerator]]
 * @see [[com.rojoma.json.io.JsonEvent]]
 */
class FusedBlockJsonEventIterator(input: Reader, fieldCache: FieldCache = IdentityFieldCache, blockSize: Int = 1024) extends BufferedIterator[JsonEvent] {
  def this(text: String) = this(new java.io.StringReader(text))

  private val block = new Array[Char](blockSize)
  private var pos = 0
  private var end = 0

  private var atTop = true // this is the value BEFORE "available" was last set
  private var available: JsonEvent = null

  private var stack = new Array[Boolean](16) // values are true for "parsing array" and false for "parsing object"
  private var stackPtr = -1

  private def push(intoArray: Boolean) {
    def growStack() {
      val newStack = new Array[Boolean](stack.length * 2)
      System.arraycopy(stack, 0, newStack, 0, stack.length)
      stack = newStack
    }
    stackPtr += 1
    if(stackPtr == stack.length) growStack()
    stack(stackPtr) = intoArray
  }

  private def pop() { assert(stackPtr >= 0); stackPtr -= 1 }

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
  private var compoundReadState: Int = _

  private var nextCharRow = 1 // This is the position of the next char returned from "nextChar()" or "peekChar()"
  private var nextCharCol = 1

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

  private def ensureChars() {
    if(atEOF) throwEOF()
  }

  private def skipChar() {
    nextChar()
  }

  private def peekChar() = {
    ensureChars()
    block(pos)
  }

  private def nextChar() = {
    val result = peekChar()
    pos += 1
    if(result == '\n') { nextCharRow += 1; nextCharCol = 1 }
    else { nextCharCol += 1 }
    result
  }

  private def skipToEndOfLine() = while(!atEOF() && peekChar() != '\n') nextChar()

  private def skipBlockComment() {
    var last = nextChar()
    while(last != '*' || peekChar() != '/') last = nextChar()
    skipChar() // skip final '/'
  }

  private def skipComment() {
    skipChar() // skip opening "/"
    peekChar() match {
      case '/' => skipChar(); skipToEndOfLine()
      case '*' => skipChar(); skipBlockComment()
      case c => lexerError(c, "/ or *", nextCharRow, nextCharCol)
    }
  }

  @annotation.tailrec
  private def skipWhitespace() {
    while(!atEOF() && Character.isWhitespace(peekChar())) skipChar()
    if(!atEOF() && peekChar() == '/') { skipComment(); skipWhitespace() }
  }

  private def advance() {
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
    val row = nextCharRow
    val col = nextCharCol
    val unpositionedToken = peekChar() match {
      case '{' => TokenOpenBrace()
      case '}' => TokenCloseBrace()
      case '[' => TokenOpenBracket()
      case ']' => TokenCloseBracket()
      case ',' => TokenComma()
      case ':' => TokenColon()
      case '"' | '\'' => TokenString(readString())
      case c if isDigit(c) || c == '-' => TokenNumber(readNumber())
      case c if Character.isUnicodeIdentifierStart(c) => TokenIdentifier(readIdentifier())
      case c => lexerError(c, expected, nextCharRow, nextCharCol)
    }
    throw new JsonUnexpectedToken(p(unpositionedToken, row, col), expected)
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
        skipChar()

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
      skipChar()

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
    nonDatum(FieldEvent(fieldCache(field)), row, col)
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
      readDatumEvent("datum or end of list")
    }
  }

  private def readArrayElementPrecededByComma(): JsonEvent = {
    skipWhitespace()
    peekChar() match {
      case ',' =>
        skipChar()

        // compat with JsonEventIterator: if the EOF happens now,
        // just end the read
        skipWhitespace()
        if(atEOF()) return null

        readDatumEvent("datum")
      case ']' =>
        endArray()
      case _ =>
        badToken("comma or end of list")
    }
  }

  private def openObject(): JsonEvent = {
    // Precondition: positioned on '{'
    val row = nextCharRow
    val col = nextCharCol
    skipChar()
    push(false)
    compoundReadState = 0
    nonDatum(StartOfObjectEvent(), row, col)
  }

  private def endObject(): JsonEvent = {
    // Precondition: positioned on '}'
    val row = nextCharRow
    val col = nextCharCol
    skipChar()
    pop()
    finishDatum(EndOfObjectEvent(), row, col)
  }

  private def openArray(): JsonEvent = {
    // Precondition: positioned on '['
    val row = nextCharRow
    val col = nextCharCol
    skipChar()
    push(true)
    compoundReadState = 0
    nonDatum(StartOfArrayEvent(), row, col)
  }

  private def endArray(): JsonEvent = {
    // Precondition: positioned on ']'
    val row = nextCharRow
    val col = nextCharCol
    skipChar()
    pop()
    finishDatum(EndOfArrayEvent(), row, col)
  }

  private def readStringEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(StringEvent(readString()), row, col)
  }

  private def readString(): String = {
    val sb = new StringBuilder
    val Boundary = nextChar()
    while(peekChar() != Boundary) {
      readPotentialSurrogatePairInto(sb, readChar(), Boundary)
    }
    nextChar() // skip closing character
    sb.toString
  }

  @annotation.tailrec
  private def readPotentialSurrogatePairInto(sb: StringBuilder, c: Char, endOfString: Char) {
    if(c >= Character.MIN_SURROGATE && c <= Character.MAX_SURROGATE) {
      val badChar = 0xfffd.toChar
      if(Character.isHighSurrogate(c)) {
        if(peekChar() == endOfString) {
          sb += badChar
        } else {
          val potentialSecondHalf = readChar()
          if(Character.isLowSurrogate(potentialSecondHalf)) {
            sb += c
            sb += potentialSecondHalf
          } else {
            sb += badChar
            readPotentialSurrogatePairInto(sb, potentialSecondHalf, endOfString)
          }
        }
      } else {
        sb += badChar
      }
    } else {
      sb += c
    }
  }

  private def readChar(): Char = {
    nextChar() match {
      case '\\' => readEscapedCharacter()
      case c => c
    }
  }

  private def readEscapedCharacter(): Char = {
    def ret(c: Char) = { skipChar(); c }
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
      case 'u' => skipChar(); readUnicodeCharacter()
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
        skipChar()
        c.toInt - '0'.toInt
      case c if 'a' <= c && c <= 'f' =>
        skipChar()
        10 + c.toInt - 'a'.toInt
      case c if 'A' <= c && c <= 'F' =>
        skipChar()
        10 + c.toInt - 'A'.toInt
      case c =>
        lexerError(c, "hex digit", nextCharRow, nextCharCol)
    }
  }

  private def readIdentifierEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(IdentifierEvent(readIdentifier()), row, col)
  }

  private def readIdentifier(): String = {
    val sb = new StringBuilder
    sb += nextChar()
    while(!atEOF() && Character.isUnicodeIdentifierPart(peekChar())) sb += nextChar()
    sb.toString()
  }

  private def readDigit(): Char = {
    if(!isDigit(peekChar)) lexerError(peekChar(), "digit", nextCharRow, nextCharCol)
    nextChar()
  }

  private def readNumberEvent(): JsonEvent = {
    val row = nextCharRow
    val col = nextCharCol
    finishDatum(NumberEvent(readNumber), row, col)
  }

  private def readNumber(): BigDecimal = {
    // JSON numbers match (a subset of) the language generated by
    // the regular expression:
    //    -?\d+(\.\d+)?([eE][+-]?\d+)?
    // We'll match the whole thing, within the limits of BigDecimal
    val sb = new StringBuilder

    val startRow = nextCharRow
    val startCol = nextCharCol

    if(peekChar() == '-') sb += nextChar()

    do { sb += readDigit() } while(!atEOF() && isDigit(peekChar()))

    val hasFrac = !atEOF() && peekChar() == '.'
    if(hasFrac) {
      sb += nextChar() // skip decimal
      do { sb += readDigit() } while(!atEOF() && isDigit(peekChar()))
    }

    val hasExponent = !atEOF() && (peekChar() == 'e' || peekChar() == 'E')
    if(hasExponent) {
      sb += nextChar() // skip e/E
      if(peekChar() == '-') sb += nextChar()
      else if(peekChar() == '+') nextChar() // just skip it
      do { sb += readDigit() } while(!atEOF() && isDigit(peekChar()))
    }

    val number = sb.toString

    try {
      BigDecimal(number, java.math.MathContext.UNLIMITED)
    } catch {
      case _: NumberFormatException =>
        throw new JsonNumberOutOfRange(number, Position(startRow, startCol))
    }
  }

  private def finishDatum(event: JsonEvent, row: Int, col: Int): JsonEvent = {
    compoundReadState = 1
    event.position = Position(row, col)
    event
  }

  private def nonDatum(event: JsonEvent, row: Int, col: Int): JsonEvent = {
    event.position = Position(row, col)
    event
  }

  private def p(token: JsonToken, row: Int, col: Int): JsonToken = {
    token.position = Position(row, col)
    token
  }

  /**
   * Finish reading the "current" object or list, where "current" is
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
        do {
          val ev = next()
          ev match {
            case StartOfObjectEvent() | StartOfArrayEvent() => count += 1
            case EndOfObjectEvent() | EndOfArrayEvent() => count -= 1
            case _ => /* nothing */
          }
        } while(count >= 0)
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
   * is the start of a list or object, `skipRestOfCompound()` is called to
   * pass over it. If it's a field event, the field and its associated value
   * are skipped. If it's the end of a list or object, no position change is
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
