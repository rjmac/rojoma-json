package com.rojoma.json.v3
package util

import ast._
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.JsonTokenGeneratorTests._

class JArrayProducerTest extends FunSuite with MustMatchers with ScalaCheckPropertyChecks {
  def r(targets: List[JValue], s: List[String]) = {
    def loop(state: JArrayProducer, targets: List[JValue], inputs: List[WrappedCharArray]): String = {
      inputs match {
        case hd :: tl =>
          state(hd) match {
            case JArrayProducer.More(newState) =>
              loop(newState, targets, tl)
            case JArrayProducer.Element(value, newState, remainingInput) =>
              value must equal (targets.head)
              loop(newState, targets.tail, remainingInput :: tl)
            case JArrayProducer.EndOfList(_, remainingInput) =>
              targets must equal (Nil)
              remainingInput.toString + tl.mkString
            case e: JArrayProducer.Error =>
              targets must equal (Nil)
              JArrayProducer.throwError(e)
          }
        case Nil =>
          state.endOfInput() match {
            case JArrayProducer.FinalEndOfList(_) =>
              targets must equal (Nil)
              ""
            case e: JArrayProducer.EndError =>
              targets must equal (Nil)
              JArrayProducer.throwError(e)
          }
      }
    }
    loop(new JArrayProducer.Builder().build, targets, s.map(WrappedCharArray.fromString))
  }

  def withSplitString(s: String)(f: List[String] => Unit): Unit = {
    forAll(splitPoints(s)) { p =>
      whenever(p.forall { i => 0 <= i && i <= s.length }) {
        f(splitAt(s, p))
      }
    }
  }

  def badRead[T: Manifest](expected: List[JValue], s: String): Unit = {
    withSplitString(s) { ss =>
      a [T] must be thrownBy { r(expected, ss) }
    }
  }

  test("Reading well-formed values") {
    forAll(splittableJson[JArray], ArbitraryValidString.arbitrary) { (jps, suffix) =>
      val (jvalue, pretty, splits) = jps
      val s = JsonUtil.renderJson(jvalue, pretty = pretty) + suffix
      whenever(splits.forall(i => 0 <= i && i <= s.length)) {
        r(jvalue.toList, splitAt(s, splits)) must equal(suffix)
      }
    }
  }

  test("Reading ill-formed values") {
    badRead[io.JsonEOF](Nil, "")
    badRead[io.JsonEOF](Nil, "[")
    badRead[io.JsonUnexpectedToken](Nil, "[,")
    badRead[io.JsonUnexpectedCharacter](List(JNumber(5), JNumber(6)), "[5,6,!]")
  }
}
