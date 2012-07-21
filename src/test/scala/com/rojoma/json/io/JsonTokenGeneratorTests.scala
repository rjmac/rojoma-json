package com.rojoma.json
package io

import ast._
import codec.JsonCodec
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._
import util.JsonUtil.renderJson
import util.WrappedCharArray

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{Gen, Arbitrary}

import scala.collection.mutable.ListBuffer

import JsonTokenGeneratorTests._

class JsonTokenGeneratorTests extends FunSuite with MustMatchers with PropertyChecks {
  def arbTest[T <: JValue : Arbitrary : JsonCodec] {
    // FIXME: Remember that in the 2.0 branch, tokens ignore their
    // positions for the purposes of considering equality.
    forAll(splittableJson[T]) { case (x, whitespace, positions) =>
      val asString = renderJson(x, pretty = whitespace)
      whenever(positions.forall { i => 0 <= i && i <= asString.length }) {
        toTokenList(splitAt(asString, positions)) must equal (new TokenIterator(new java.io.StringReader(asString)).toList)
      }
    }
  }

  def withSplitString(s: String)(f: Seq[String] => Unit) {
    forAll(splitPoints(s)) { positions =>
      whenever(positions.forall { i => 0 <= i && i <= s.length }) {
        f(splitAt(s, positions))
      }
    }
  }

  test("JsonTokenGenerator gives the same result as TokenIterator for valid inputs") {
    // This is a good random test, but the space is large enough that
    // it's not really sufficient.  And of course it only checks
    // positive cases.  Thus all the other tests below.  We want this
    // one mainly for checking the output positions.
    arbTest[JValue]
  }

  test("JsonTokenGenerator gives the same result as TokenIterator for atoms") {
    // this also checks identifiers, albeit not super well since it's
    // limited to true, false, and null.
    arbTest[JAtom]
  }

  test("reading a token leaves the first not-part-of-the-token unconsumed") {
    def tc(jsonFragment: String, expected: JsonToken, remaining: String) {
      withSplitString(jsonFragment) { fragments =>
        firstToken(fragments) must be ((expected, remaining))
      }
    }

    tc("\"hello\" gnu", TokenString("hello"), " gnu")
    tc("true gnu", TokenIdentifier("true"), " gnu")
    tc("1.432 gnu", TokenNumber(BigDecimal("1.432")), " gnu")
    tc("[ gnu", TokenOpenBracket, " gnu")
    tc("] gnu", TokenCloseBracket, " gnu")
    tc("{ gnu", TokenOpenBrace, " gnu")
    tc("} gnu", TokenCloseBrace, " gnu")
    tc(": gnu", TokenColon, " gnu")
    tc(", gnu", TokenComma, " gnu")
  }

  test("EOF is allowed inside a line comment") {
    withSplitString("// eof here --->") { fragments =>
      evaluating { firstToken(fragments) } must produce [NoSuchTokenException]
    }
  }

  test("EOF is not allowed inside a block comment") {
    withSplitString("/* eof here --->") { fragments =>
      evaluating { firstToken(fragments) } must produce [JsonEOF]
    }
  }

  def t(jsonFragment: String, expected: JsonToken*) {
    withSplitString(jsonFragment) { fragments =>
      toTokenList(fragments).map(_.token) must equal (expected)
    }
  }

  test("multiple tokens can be read without any intervening space") {
    t("\"hello\":", TokenString("hello"), TokenColon)
    t("hello:", TokenIdentifier("hello"), TokenColon)
    t("123:", TokenNumber(BigDecimal(123)), TokenColon)
    t("[:", TokenOpenBracket, TokenColon)
    t("]:", TokenCloseBracket, TokenColon)
    t("{:", TokenOpenBrace, TokenColon)
    t("}:", TokenCloseBrace, TokenColon)
    t("::", TokenColon, TokenColon)
    t(",:", TokenComma, TokenColon)
  }

  test("multiple tokens can be read with one intervening space") {
    t("\"hello\" :", TokenString("hello"), TokenColon)
    t("hello :", TokenIdentifier("hello"), TokenColon)
    t("123 :", TokenNumber(BigDecimal(123)), TokenColon)
    t("[ :", TokenOpenBracket, TokenColon)
    t("] :", TokenCloseBracket, TokenColon)
    t("{ :", TokenOpenBrace, TokenColon)
    t("} :", TokenCloseBrace, TokenColon)
    t(": :", TokenColon, TokenColon)
    t(", :", TokenComma, TokenColon)
  }

  test("multiple tokens can be read with multiple intervening space") {
    t("\"hello\"  :", TokenString("hello"), TokenColon)
    t("hello \n:", TokenIdentifier("hello"), TokenColon)
    t("123 /*hello*/:", TokenNumber(BigDecimal(123)), TokenColon)
    t("[ //gnu\n:", TokenOpenBracket, TokenColon)
    t("] /*hello*/ :", TokenCloseBracket, TokenColon)
    t("{ //gnu\n   :", TokenOpenBrace, TokenColon)
    t("} \t\t\t:", TokenCloseBrace, TokenColon)
    t(":/*bleh*/ :", TokenColon, TokenColon)
    t(",// gnu\n  :", TokenComma, TokenColon)
  }

  test("reading replaces broken surrogate pairs") {
    t("'\ud800'", TokenString("\ufffd"))
    t("'\ud800x'", TokenString("\ufffdx"))
    t("'\udc00'", TokenString("\ufffd"))
    t("'\udc00x'", TokenString("\ufffdx"))
    t("'\udc00\ud800\udc00'", TokenString("\ufffd\ud800\udc00"))

    t("'\\ud800'", TokenString("\ufffd"))
    t("'\\ud800x'", TokenString("\ufffdx"))
    t("'\\udc00'", TokenString("\ufffd"))
    t("'\\udc00x'", TokenString("\ufffdx"))
    t("'\\udc00\\ud800\\udc00'", TokenString("\ufffd\ud800\udc00"))
  }

  test("reading handles mixed escaped/unescaped surrogate pairs") {
    t("'\\ud800\udc00'", TokenString("\ud800\udc00"))
    t("'\ud800\\udc00'", TokenString("\ud800\udc00"))
  }
}

object JsonTokenGeneratorTests {
  def toTokenList(in: Seq[String]): Seq[PositionedJsonToken] = {
    val b = new ListBuffer[PositionedJsonToken]
    def loop(lexer: JsonTokenGenerator, chunk: WrappedCharArray): JsonTokenGenerator = {
      lexer.lex(chunk) match {
        case JsonTokenGenerator.Token(token, newGenerator, remaining) =>
          b += token
          loop(newGenerator, remaining)
        case JsonTokenGenerator.More(newGenerator) =>
          newGenerator
      }
    }
    val finalLexer = in.map(WrappedCharArray(_)).foldLeft(JsonTokenGenerator.newGenerator)(loop)
    finalLexer.finish() match {
      case JsonTokenGenerator.FinalToken(token, _, _) => b += token
      case JsonTokenGenerator.EndOfInput(_, _) => /* pass */
    }
    b.toList
  }

  def splitAt(text: String, positions: List[Int]): List[String] =
    (List(0) ++ positions.sorted ++ List(text.length)).sliding(2).map { case Seq(a,b) =>
      text.substring(a, b)
    }.toList

  def splittableJson[T <: JValue : Arbitrary : JsonCodec] = for {
    pretty <- Arbitrary.arbitrary[Boolean]
    jvalue <- Arbitrary.arbitrary[T]
    splitCount <- Gen.choose(0, 10)
    splits <- Gen.listOfN(splitCount, Gen.choose(0, renderJson(jvalue, pretty).length))
  } yield (jvalue, pretty, splits)

  def splitPoints(s: String) = for {
    splitCount <- Gen.choose(0, 10)
    splits <- Gen.listOfN(splitCount, Gen.choose(0, s.length))
  } yield splits

  def firstToken(s: Seq[String]) = {
    def loop(lexer: JsonTokenGenerator, strings: List[WrappedCharArray]): (JsonToken, String) = {
      strings match {
        case hd :: tl =>
          lexer.lex(hd) match {
            case JsonTokenGenerator.Token(t, _, remaining) => (t.token, (remaining :: tl).mkString)
            case JsonTokenGenerator.More(l) => loop(l, tl)
          }
        case Nil =>
          lexer.finish() match {
            case JsonTokenGenerator.FinalToken(t, _, _) => (t.token, "")
            case JsonTokenGenerator.EndOfInput(r, c) => throw new NoSuchTokenException(r, c)
          }
      }
    }
    loop(JsonTokenGenerator.newGenerator, s.map(WrappedCharArray(_)).toList)
  }
}
