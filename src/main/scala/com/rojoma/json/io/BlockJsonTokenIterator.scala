package com.rojoma.json
package io

import util.WrappedCharArray

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
    else {
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
          hasNext
      }
    }
}
