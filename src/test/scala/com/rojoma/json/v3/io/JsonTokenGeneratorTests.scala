package com.rojoma.json.v3
package io

import ast._
import codec.JsonEncode
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._
import util.JsonUtil.renderJson
import util.WrappedCharArray

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import org.scalacheck.{Gen, Arbitrary}

import scala.collection.mutable.ListBuffer

import JsonTokenGeneratorTests._
import Tokens._

class JsonTokenGeneratorTests extends FunSuite with MustMatchers with ScalaCheckPropertyChecks {
  def arbTest[T <: JValue : Arbitrary : JsonEncode] {
    forAll(splittableJson[T]) { case (x, whitespace, positions) =>
      val asString = renderJson(x, pretty = whitespace)
      whenever(positions.forall { i => 0 <= i && i <= asString.length }) {
        val generatedTokenList = toTokenList(splitAt(asString, positions))
        generatedTokenList.forall(_.position.isValid) must be (true)
        val iteratorTokenList = new JsonTokenIterator(new java.io.StringReader(asString)).toList
        generatedTokenList.map { t => (t, t.position) } must equal (iteratorTokenList.map { t => (t, t.position) })
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

    tc("\"hello\" gnu", tokenString("hello"), " gnu")
    tc("true gnu", tokenIdentifier("true"), " gnu")
    tc("1.432 gnu", tokenNumber("1.432"), " gnu")
    tc("[ gnu", tokenOpenBracket(), " gnu")
    tc("] gnu", tokenCloseBracket(), " gnu")
    tc("{ gnu", tokenOpenBrace(), " gnu")
    tc("} gnu", tokenCloseBrace(), " gnu")
    tc(": gnu", tokenColon(), " gnu")
    tc(", gnu", tokenComma(), " gnu")
  }

  test("EOF is allowed inside a line comment") {
    withSplitString("// eof here --->") { fragments =>
      a [NoSuchTokenException] must be thrownBy { firstToken(fragments) }
    }
  }

  test("EOF is not allowed inside a block comment") {
    withSplitString("/* eof here --->") { fragments =>
      a [JsonEOF] must be thrownBy { firstToken(fragments) }
    }
  }

  def t(jsonFragment: String, expected: JsonToken*) {
    withSplitString(jsonFragment) { fragments =>
      toTokenList(fragments) must equal (expected)
    }
  }

  test("multiple tokens can be read without any intervening space") {
    t("\"hello\":", tokenString("hello"), tokenColon())
    t("hello:", tokenIdentifier("hello"), tokenColon())
    t("123:", tokenNumber(123), tokenColon())
    t("[:", tokenOpenBracket(), tokenColon())
    t("]:", tokenCloseBracket(), tokenColon())
    t("{:", tokenOpenBrace(), tokenColon())
    t("}:", tokenCloseBrace(), tokenColon())
    t("::", tokenColon(), tokenColon())
    t(",:", tokenComma(), tokenColon())
  }

  test("multiple tokens can be read with one intervening space") {
    t("\"hello\" :", tokenString("hello"), tokenColon())
    t("hello :", tokenIdentifier("hello"), tokenColon())
    t("123 :", tokenNumber(123), tokenColon())
    t("[ :", tokenOpenBracket(), tokenColon())
    t("] :", tokenCloseBracket(), tokenColon())
    t("{ :", tokenOpenBrace(), tokenColon())
    t("} :", tokenCloseBrace(), tokenColon())
    t(": :", tokenColon(), tokenColon())
    t(", :", tokenComma(), tokenColon())
  }

  test("multiple tokens can be read with multiple intervening space") {
    t("\"hello\"  :", tokenString("hello"), tokenColon())
    t("hello \n:", tokenIdentifier("hello"), tokenColon())
    t("123 /*hello*/:", tokenNumber(123), tokenColon())
    t("[ //gnu\n:", tokenOpenBracket(), tokenColon())
    t("] /*hello*/ :", tokenCloseBracket(), tokenColon())
    t("{ //gnu\n   :", tokenOpenBrace(), tokenColon())
    t("} \t\t\t:", tokenCloseBrace(), tokenColon())
    t(":/*bleh*/ :", tokenColon(), tokenColon())
    t(",// gnu\n  :", tokenComma(), tokenColon())
  }

  test("reading replaces broken surrogate pairs") {
    t("'\ud800'", tokenString("\ufffd"))
    t("'\ud800x'", tokenString("\ufffdx"))
    t("'\udc00'", tokenString("\ufffd"))
    t("'\udc00x'", tokenString("\ufffdx"))
    t("'\udc00\ud800\udc00'", tokenString("\ufffd\ud800\udc00"))

    t("'\\ud800'", tokenString("\ufffd"))
    t("'\\ud800x'", tokenString("\ufffdx"))
    t("'\\udc00'", tokenString("\ufffd"))
    t("'\\udc00x'", tokenString("\ufffdx"))
    t("'\\udc00\\ud800\\udc00'", tokenString("\ufffd\ud800\udc00"))
  }

  test("reading handles mixed escaped/unescaped surrogate pairs") {
    t("'\\ud800\udc00'", tokenString("\ud800\udc00"))
    t("'\ud800\\udc00'", tokenString("\ud800\udc00"))
  }
}

object JsonTokenGeneratorTests {
  def toTokenList(in: Seq[String]): Seq[JsonToken] = {
    val b = new ListBuffer[JsonToken]
    def loop(lexer: JsonTokenGenerator, chunk: WrappedCharArray): JsonTokenGenerator = {
      lexer.lex(chunk) match {
        case JsonTokenGenerator.Token(token, newGenerator, remaining) =>
          b += token
          loop(newGenerator, remaining)
        case JsonTokenGenerator.More(newGenerator) =>
          newGenerator
      }
    }
    val finalLexer = in.map(WrappedCharArray.fromString).foldLeft(JsonTokenGenerator.newGenerator)(loop)
    finalLexer.finish() match {
      case JsonTokenGenerator.FinalToken(token, _) => b += token
      case JsonTokenGenerator.EndOfInput(_) => /* pass */
    }
    b.toList
  }

  def splitAt(text: String, positions: List[Int]): List[String] =
    (List(0) ++ positions.sorted ++ List(text.length)).sliding(2).map { case Seq(a,b) =>
      text.substring(a, b)
    }.toList

  def splittableJson[T <: JValue : Arbitrary : JsonEncode] = for {
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
            case JsonTokenGenerator.Token(t, _, remaining) => (t, (remaining :: tl).mkString)
            case JsonTokenGenerator.More(l) => loop(l, tl)
          }
        case Nil =>
          lexer.finish() match {
            case JsonTokenGenerator.FinalToken(t, _) => (t, "")
            case JsonTokenGenerator.EndOfInput(p) => throw new NoSuchTokenException(p)
          }
      }
    }
    loop(JsonTokenGenerator.newGenerator, s.map(WrappedCharArray.fromString).toList)
  }
}
