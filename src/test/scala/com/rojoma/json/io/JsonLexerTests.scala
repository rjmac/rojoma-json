package com.rojoma.json
package io

import ast._
import codec.JsonCodec
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._
import util.JsonUtil.renderJson

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{Gen, Arbitrary}

import scala.collection.mutable.ListBuffer

class JsonLexerTests extends FunSuite with MustMatchers with PropertyChecks {
  def toTokenList(in: Seq[String]): Seq[PositionedJsonToken] = {
    val b = new ListBuffer[PositionedJsonToken]
    def loop(lexer: JsonLexer, chunk: String): JsonLexer = {
      lexer.lex(chunk) match {
        case JsonLexer.Token(token, newLexer, remaining) =>
          b += token
          loop(newLexer, remaining)
        case JsonLexer.More(newLexer) =>
          newLexer
      }
    }
    val finalLexer = in.foldLeft(JsonLexer.newLexer)(loop)
    finalLexer.finish() match {
      case JsonLexer.FinalToken(token, _, _) => b += token
      case JsonLexer.EndOfInput(_, _) => /* pass */
    }
    b.toList
  }

  def splitAt(text: String, positions: List[Int]): List[String] =
    (List(0) ++ positions.sorted ++ List(text.length)).sliding(2).map { case Seq(a,b) =>
      text.substring(a, b)
    }.toList

  def splitString[T <: JValue : Arbitrary : JsonCodec] = for {
    pretty <- Arbitrary.arbitrary[Boolean]
    jvalue <- Arbitrary.arbitrary[T]
    splitCount <- Gen.choose(0, 10)
    splits <- Gen.listOfN(splitCount, Gen.choose(0, renderJson(jvalue, pretty).length))
  } yield (jvalue, pretty, splits)

  def arbTest[T <: JValue : Arbitrary : JsonCodec] {
    // FIXME: Remember that in the 2.0 branch, tokens ignore their
    // positions for the purposes of considering equality.
    forAll(splitString[T]) { case (x, whitespace, positions) =>
      val asString = renderJson(x, pretty = whitespace)
      whenever(positions.forall { i => 0 <= i && i <= asString.length }) {
        try {
          toTokenList(splitAt(asString, positions)) must equal (new TokenIterator(new java.io.StringReader(asString)).toList)
        } catch {
          case e => println(e); throw e
        }
      }
    }
  }

  test("JsonLexer gives the same result as TokenIterator for valid inputs") {
    // This is a good random test, but the space is large enough that
    // it's not really sufficient.  And of course it only checks
    // positive cases.  Thus all the other tests below.  We want this
    // one mainly for checking the output positions.
    arbTest[JValue]
  }

  test("JsonLexer gives the same result as TokenIterator for atoms") {
    // this also checks identifiers, albeit not super well since it's
    // limited to true, false, and null.
    arbTest[JAtom]
  }
}
