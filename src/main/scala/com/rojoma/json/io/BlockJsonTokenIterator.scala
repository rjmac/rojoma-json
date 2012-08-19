package com.rojoma.json
package io

import scala.annotation.tailrec

import util.WrappedCharArray

/** Convert a character-stream into a token-stream.
  *
  * This reads characters in blocks of size `blockSize`, so if the
  * underlying `Reader` is to be re-used for other non-JSON purposes
  * after finishing reading, a [[com.rojoma.json.io.JsonTokenIterator]]
  * should be used instead.
  *
  * As extensions to standard JSON, this reader supports single-quoted
  * strings and Javascript-style comments.
  *
  * @see [[com.rojoma.json.io.JsonTokenIterator]]
  * @see [[com.rojoma.json.io.JsonToken]]
  */
class BlockJsonTokenIterator(reader: java.io.Reader, blockSize: Int = 1024) extends BufferedIterator[JsonToken] {
  private val buf = new Array[Char](blockSize)
  private var lexer = JsonTokenGenerator.newGenerator
  private var remaining: WrappedCharArray = null
  private var token: JsonToken = null
  private var lastPos = Position.Invalid

  private def fillRemaining() {
    reader.read(buf) match {
      case -1 => remaining = null
      case n => remaining = WrappedCharArray(buf, 0, n)
    }
  }

  def head =
    if(hasNext) token
    else throw new NoSuchTokenException(lastPos)

  def next() = {
    val result = head
    token = null
    result
  }

  def hasNext: Boolean =
    if(token != null) true
    else if(lastPos.isValid) false
    else advance()

  @tailrec
  private def advance(): Boolean = {
    if(remaining == null) {
      fillRemaining()
      if(remaining == null) {
        lexer.finish() match {
          case JsonTokenGenerator.EndOfInput(pos) =>
            lastPos = pos
            return false
          case JsonTokenGenerator.FinalToken(t, pos) =>
            token = t
            lastPos = pos
            return true
        }
      }
    }
    // ok, remaining != null...
    lexer.lex(remaining) match {
      case JsonTokenGenerator.Token(t, newLexer, newRemaining) =>
        token = t
        lexer = newLexer
        if(newRemaining.isEmpty) remaining = null
        else remaining = newRemaining
        true
      case JsonTokenGenerator.More(newLexer) =>
        lexer = newLexer
        remaining = null
        advance()
    }
  }
}
