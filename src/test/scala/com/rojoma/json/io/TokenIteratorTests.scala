package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class TokenIteratorTests extends FunSuite with MustMatchers {
  def r(s: String) = new java.io.StringReader(s)
  
  def t(s: String) = new TokenIterator(r(s)).next().token

  test("reading single tokens succeeds") {
    t("\"hello\"") must equal (TokenString("hello"))
    t("true") must equal (TokenIdentifier("true"))
    t("1.432") must equal (TokenNumber(BigDecimal("1.432")))
    t("[") must equal (TokenOpenBracket)
    t("]") must equal (TokenCloseBracket)
    t("{") must equal (TokenOpenBrace)
    t("}") must equal (TokenCloseBrace)
    t(":") must equal (TokenColon)
    t(",") must equal (TokenComma)
  }

  test("reading a token leaves the reader positioned at the first unambiguously-not-part-of-the-token character") {
    def tc(s: String) = {
      val reader = r(s)
      val tok = new TokenIterator(reader).next().token
      (tok, reader.read().toChar)
    }

    tc("\"hello\" gnu") must be (TokenString("hello"), ' ')
    tc("true gnu") must be (TokenIdentifier("true"), 'g')
    tc("1.432 gnu") must be (TokenNumber(BigDecimal("1.432")), 'g')
    tc("[ gnu") must be (TokenOpenBracket, ' ')
    tc("] gnu") must be (TokenCloseBracket, ' ')
    tc("{ gnu") must be (TokenOpenBrace, ' ')
    tc("} gnu") must be (TokenCloseBrace, ' ')
    tc(": gnu") must be (TokenColon, ' ')
    tc(", gnu") must be (TokenComma, ' ')
  }

  test("EOF is allowed inside a line comment") {
    t("// eof here --->") must equal (TokenEOF)
  }

  test("EOF is not allowed inside a block comment") {
    evaluating { t("/* eof here --->") } must produce [JsonEOF]
  }

  test("multiple tokens can be read without any intervening space") {
    def l(s: String) = new TokenIterator(r(s)).map(_.token).toList
    l("\"hello\":") must equal (List(TokenString("hello"), TokenColon, TokenEOF))
    l("hello:") must equal (List(TokenIdentifier("hello"), TokenColon, TokenEOF))
    l("123:") must equal (List(TokenNumber(BigDecimal(123)), TokenColon, TokenEOF))
    l("[:") must equal (List(TokenOpenBracket, TokenColon, TokenEOF))
    l("]:") must equal (List(TokenCloseBracket, TokenColon, TokenEOF))
    l("{:") must equal (List(TokenOpenBrace, TokenColon, TokenEOF))
    l("}:") must equal (List(TokenCloseBrace, TokenColon, TokenEOF))
    l("::") must equal (List(TokenColon, TokenColon, TokenEOF))
    l(",:") must equal (List(TokenComma, TokenColon, TokenEOF))
  }

  test("multiple tokens can be read with one intervening space") {
    def l(s: String) = new TokenIterator(r(s)).map(_.token).toList
    l("\"hello\" :") must equal (List(TokenString("hello"), TokenColon, TokenEOF))
    l("hello :") must equal (List(TokenIdentifier("hello"), TokenColon, TokenEOF))
    l("123 :") must equal (List(TokenNumber(BigDecimal(123)), TokenColon, TokenEOF))
    l("[ :") must equal (List(TokenOpenBracket, TokenColon, TokenEOF))
    l("] :") must equal (List(TokenCloseBracket, TokenColon, TokenEOF))
    l("{ :") must equal (List(TokenOpenBrace, TokenColon, TokenEOF))
    l("} :") must equal (List(TokenCloseBrace, TokenColon, TokenEOF))
    l(": :") must equal (List(TokenColon, TokenColon, TokenEOF))
    l(", :") must equal (List(TokenComma, TokenColon, TokenEOF))
  }

  test("multiple tokens can be read with multiple intervening space") {
    def l(s: String) = new TokenIterator(r(s)).map(_.token).toList
    l("\"hello\"  :") must equal (List(TokenString("hello"), TokenColon, TokenEOF))
    l("hello \n:") must equal (List(TokenIdentifier("hello"), TokenColon, TokenEOF))
    l("123 /*hello*/:") must equal (List(TokenNumber(BigDecimal(123)), TokenColon, TokenEOF))
    l("[ //gnu\n:") must equal (List(TokenOpenBracket, TokenColon, TokenEOF))
    l("] /*hello*/ :") must equal (List(TokenCloseBracket, TokenColon, TokenEOF))
    l("{ //gnu\n   :") must equal (List(TokenOpenBrace, TokenColon, TokenEOF))
    l("} \t\t\t:") must equal (List(TokenCloseBrace, TokenColon, TokenEOF))
    l(":/*bleh*/ :") must equal (List(TokenColon, TokenColon, TokenEOF))
    l(",// gnu\n  :") must equal (List(TokenComma, TokenColon, TokenEOF))
  }
}