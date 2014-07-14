package com.rojoma.json.v3
package util

import ast._
import testsupport.ArbitraryJValue._
import testsupport.ArbitraryValidString._

import org.scalacheck.{Gen, Arbitrary}

import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.PropertyChecks

import io.JsonTokenGeneratorTests._

class JValueProducerTest extends FunSuite with MustMatchers with PropertyChecks {
  def r(s: List[String]) = {
    def loop(state: JValueProducer, inputs: List[WrappedCharArray]): (JValue, String) = {
      inputs match {
        case hd :: tl =>
          state.consume(hd) match {
            case JValueProducer.More(newState) => loop(newState, tl)
            case JValueProducer.Value(value, _, remainingInput) => (value, remainingInput.toString ++ tl.mkString)
          }
        case Nil =>
          state.finish() match {
            case JValueProducer.FinalValue(value, _) => (value, "")
          }
      }
    }
    loop(new JValueProducer.Builder().build, s.map(WrappedCharArray.fromString))
  }

  def withSplitString(s: String)(f: List[String] => Unit) {
    forAll(splitPoints(s)) { p =>
      whenever(p.forall { i => 0 <= i && i <= s.length }) {
        f(splitAt(s, p))
      }
    }
  }

  def badRead[T: Manifest](s: String) {
    withSplitString(s) { ss =>
      a [T] must be thrownBy { r(ss) }
    }
  }

  def checkRead(s: String, target: JValue, remainder: String = "") {
    withSplitString(s) { ss =>
      r(ss) must equal ((target, remainder))
    }
  }

  test("Reading well-formed values") {
    forAll(splittableJson[JValue], ArbitraryValidString.arbitrary) { (jps, suffix) =>
      val (jvalue, pretty, splits) = jps
      val trueSuffix = if(jvalue.isInstanceOf[JNull] || jvalue.isInstanceOf[JBoolean] || jvalue.isInstanceOf[JNumber]) " " + suffix
                       else suffix
      val s = JsonUtil.renderJson(jvalue, pretty = pretty) + trueSuffix
      whenever(splits.forall(i => 0 <= i && i <= s.length)) {
        r(splitAt(s, splits)) must equal ((jvalue, trueSuffix))
      }
    }
  }

  test("Reading ill-formed values") {
    badRead[io.JsonEOF]("")
    badRead[io.JsonEOF]("[")
    badRead[io.JsonUnexpectedToken]("[,")
    badRead[io.JsonUnexpectedCharacter]("[5,6,!]")
  }
}
