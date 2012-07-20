package com.rojoma.json
package io

import util.WrappedCharArray

class BlockTokenIterator(reader: java.io.Reader, blockSize: Int = 1024) extends BufferedIterator[PositionedJsonToken] {
  private val buf = new Array[Char](blockSize)
  private var lexer = JsonLexer.newLexer
  private var remaining: WrappedCharArray = null
  private var token: PositionedJsonToken = null
  private var lastRow, lastCol = -1

  private def fillRemaining() {
    reader.read(buf) match {
      case -1 => remaining = null
      case n => remaining = WrappedCharArray(buf, 0, n)
    }
  }

  def head =
    if(hasNext) token
    else throw new NoSuchTokenException(lastRow, lastCol)

  def next() = {
    val result = head
    token = null
    result
  }

  def hasNext: Boolean =
    if(token != null) true
    else if(lastRow != -1) false
    else {
      if(remaining == null) {
        fillRemaining()
        if(remaining == null) {
          lexer.finish() match {
            case JsonLexer.EndOfInput(r, c) =>
              lastRow = r
              lastCol = c
              return false
            case JsonLexer.FinalToken(t, r, c) =>
              token = t
              lastRow = r
              lastCol = c
              return true
          }
        }
      }
      // ok, remaining != null...
      lexer.lex(remaining) match {
        case JsonLexer.Token(t, newLexer, newRemaining) =>
          token = t
          lexer = newLexer
          if(newRemaining.isEmpty) remaining = null
          else remaining = newRemaining
          true
        case JsonLexer.More(newLexer) =>
          lexer = newLexer
          remaining = null
          hasNext
      }
    }
}
