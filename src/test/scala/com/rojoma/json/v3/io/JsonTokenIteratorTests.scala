package com.rojoma.json.v3
package io

import org.scalatest.{FunSuite, MustMatchers}

import Tokens._

class JsonTokenIteratorTests extends FunSuite with MustMatchers {
  def r(s: String) = new java.io.StringReader(s)
  
  def t(s: String) = new JsonTokenIterator(r(s)).next()

  test("reading single tokens succeeds") {
    t("\"hello\"") must equal (tokenString("hello"))
    t("true") must equal (tokenIdentifier("true"))
    t("1.432") must equal (tokenNumber("1.432"))
    t("[") must equal (tokenOpenBracket())
    t("]") must equal (tokenCloseBracket())
    t("{") must equal (tokenOpenBrace())
    t("}") must equal (tokenCloseBrace())
    t(":") must equal (tokenColon())
    t(",") must equal (tokenComma())
  }

  test("reading a token leaves the reader positioned at the first unambiguously-not-part-of-the-token character") {
    def tc(s: String) = {
      val reader = r(s)
      val tok = new JsonTokenIterator(reader).next()
      (tok, reader.read().toChar)
    }

    tc("\"hello\" gnu") must be (tokenString("hello"), ' ')
    tc("true gnu") must be (tokenIdentifier("true"), 'g')
    tc("1.432 gnu") must be (tokenNumber("1.432"), 'g')
    tc("[ gnu") must be (tokenOpenBracket(), ' ')
    tc("] gnu") must be (tokenCloseBracket(), ' ')
    tc("{ gnu") must be (tokenOpenBrace(), ' ')
    tc("} gnu") must be (tokenCloseBrace(), ' ')
    tc(": gnu") must be (tokenColon(), ' ')
    tc(", gnu") must be (tokenComma(), ' ')
  }

  test("EOF is allowed inside a line comment") {
    a [NoSuchTokenException] must be thrownBy { t("// eof here --->") }
  }

  test("EOF is not allowed inside a block comment") {
    a [JsonEOF] must be thrownBy { t("/* eof here --->") }
    a [JsonLexException] must be thrownBy { t("/* eof here --->") }
  }

  test("EOF is not allowed inside a string") {
    a [JsonEOF] must be thrownBy { t("'eof here --->") }
    a [JsonLexException] must be thrownBy { t("'eof here --->") }
  }

  test("multiple tokens can be read without any intervening space") {
    def l(s: String) = new JsonTokenIterator(r(s)).toList
    l("\"hello\":") must equal (List(tokenString("hello"), tokenColon()))
    l("hello:") must equal (List(tokenIdentifier("hello"), tokenColon()))
    l("123:") must equal (List(tokenNumber(123), tokenColon()))
    l("[:") must equal (List(tokenOpenBracket(), tokenColon()))
    l("]:") must equal (List(tokenCloseBracket(), tokenColon()))
    l("{:") must equal (List(tokenOpenBrace(), tokenColon()))
    l("}:") must equal (List(tokenCloseBrace(), tokenColon()))
    l("::") must equal (List(tokenColon(), tokenColon()))
    l(",:") must equal (List(tokenComma(), tokenColon()))
  }

  test("multiple tokens can be read with one intervening space") {
    def l(s: String) = new JsonTokenIterator(r(s)).toList
    l("\"hello\" :") must equal (List(tokenString("hello"), tokenColon()))
    l("hello :") must equal (List(tokenIdentifier("hello"), tokenColon()))
    l("123 :") must equal (List(tokenNumber(123), tokenColon()))
    l("[ :") must equal (List(tokenOpenBracket(), tokenColon()))
    l("] :") must equal (List(tokenCloseBracket(), tokenColon()))
    l("{ :") must equal (List(tokenOpenBrace(), tokenColon()))
    l("} :") must equal (List(tokenCloseBrace(), tokenColon()))
    l(": :") must equal (List(tokenColon(), tokenColon()))
    l(", :") must equal (List(tokenComma(), tokenColon()))
  }

  test("multiple tokens can be read with multiple intervening space") {
    def l(s: String) = new JsonTokenIterator(r(s)).toList
    l("\"hello\"  :") must equal (List(tokenString("hello"), tokenColon()))
    l("hello \n:") must equal (List(tokenIdentifier("hello"), tokenColon()))
    l("123 /*hello*/:") must equal (List(tokenNumber(123), tokenColon()))
    l("[ //gnu\n:") must equal (List(tokenOpenBracket(), tokenColon()))
    l("] /*hello*/ :") must equal (List(tokenCloseBracket(), tokenColon()))
    l("{ //gnu\n   :") must equal (List(tokenOpenBrace(), tokenColon()))
    l("} \t\t\t:") must equal (List(tokenCloseBrace(), tokenColon()))
    l(":/*bleh*/ :") must equal (List(tokenColon(), tokenColon()))
    l(",// gnu\n  :") must equal (List(tokenComma(), tokenColon()))
  }

  test("reading replaces broken surrogate pairs") {
    t("'\ud800'") must equal (tokenString("\ufffd"))
    t("'\ud800x'") must equal (tokenString("\ufffdx"))
    t("'\udc00'") must equal (tokenString("\ufffd"))
    t("'\udc00x'") must equal (tokenString("\ufffdx"))
    t("'\udc00\ud800\udc00'") must equal (tokenString("\ufffd\ud800\udc00"))

    t("'\\ud800'") must equal (tokenString("\ufffd"))
    t("'\\ud800x'") must equal (tokenString("\ufffdx"))
    t("'\\udc00'") must equal (tokenString("\ufffd"))
    t("'\\udc00x'") must equal (tokenString("\ufffdx"))
    t("'\\udc00\\ud800\\udc00'") must equal (tokenString("\ufffd\ud800\udc00"))
  }

  test("reading handles mixed escaped/unescaped surrogate pairs") {
    t("'\\ud800\udc00'") must equal (tokenString("\ud800\udc00"))
    t("'\ud800\\udc00'") must equal (tokenString("\ud800\udc00"))
  }
}
