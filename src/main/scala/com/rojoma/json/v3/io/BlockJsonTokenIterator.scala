package com.rojoma.json.v3
package io

import scala.annotation.tailrec

import java.io.{Reader, StringReader}

import `-impl`.util.AbstractBufferedIterator
import util.WrappedCharArray

/** Convert a character-stream into a token-stream.
  *
  * This reads characters in blocks of size `blockSize`, so if the
  * underlying `Reader` is to be re-used for other non-JSON purposes
  * after finishing reading, a [[com.rojoma.json.v3.io.JsonTokenIterator]]
  * should be used instead.
  *
  * As extensions to standard JSON, this reader supports single-quoted
  * strings and Javascript-style comments.
  *
  * @see [[com.rojoma.json.v3.io.JsonTokenIterator]]
  * @see [[com.rojoma.json.v3.io.JsonTokenGenerator]]
  * @see [[com.rojoma.json.v3.io.JsonToken]]
  */
class BlockJsonTokenIterator private (private var remaining: WrappedCharArray, reader: Reader, buf: Array[Char]) extends AbstractBufferedIterator[JsonToken] {
  def this(reader: Reader, blockSize: Int = 1024) = this(null, reader, new Array[Char](blockSize))
  def this(text: String) = this(new StringReader(text), 1024)

  private var lexer = JsonTokenGenerator.newGenerator
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

  override def toString =
    if(token ne null) "non-empty iterator"
    else if(lastPos.isValid) "empty iterator"
    else "possibly-empty iterator"

  def hasNext: Boolean =
    if(token ne null) true
    else if(lastPos.isValid) false
    else advance()

  @tailrec
  private def advance(): Boolean = {
    if(remaining eq null) {
      fillRemaining()
      if(remaining eq null) {
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
