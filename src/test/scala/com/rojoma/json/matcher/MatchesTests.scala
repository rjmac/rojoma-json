package com.rojoma.json
package matcher

import ast._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class MatchesTests extends FunSuite with MustMatchers {
  import Pattern._

  def j(s: String) = io.JsonReader.fromString(s)

  test("atom literals match") {
    (JNull matches JNull) must equal (Some(Map.empty))
    (JBoolean(true) matches JBoolean(true)) must equal (Some(Map.empty))
    (JBoolean(false) matches JBoolean(false)) must equal (Some(Map.empty))
    (JString("hello") matches JString("hello")) must equal (Some(Map.empty))
  }

  test("atom literals don't match") {
    (JNull matches JBoolean(true)) must equal (None)
    (JBoolean(true) matches JBoolean(false)) must equal (None)
    (JString("hello") matches JString("world")) must equal (None)
  }

  test("variables get filled in") {
    val x = Variable.raw[JBoolean]()
    (x matches JBoolean(true)) must equal (Some(Map(x -> JBoolean(true))))
    (x matches JBoolean(false)) must equal (Some(Map(x -> JBoolean(false))))
  }

  test("variables don't match") {
    val x = Variable.raw[JString]()
    (x matches JBoolean(true)) must equal (None)
  }

  test("sequence literals match exactly") {
    (j("""[1,2,3]""") matches j("""[1,2,3]""")) must equal (Some(Map.empty))
  }

  test("sequence literals match prefix") {
    (j("""[1,2,3]""") matches j("""[1,2,3,4,5]""")) must equal (Some(Map.empty))
  }

  test("sequence literals do not match overlong") {
    (j("""[1,2,3]""") matches j("""[1,2]""")) must equal (None)
  }

  test("sequence literals to not match mismatch") {
    (j("""[1,2,3]""") matches j("""[1,3,3]""")) must equal (None)
  }

  test("sequence variables match") {
    val middle = Variable.raw[JIntegral]()
    (PArray(1, middle, 3) matches j("""[1,2,3]""")) must equal (Some(Map(middle -> JIntegral(2))))
  }

  test("nested sequence variables match") {
    val a = Variable.raw[JIntegral]()
    val b = Variable.raw[JString]()
    (PArray(1, a, PArray("hello", b, "world"), 3) matches j("""[1,2,["hello","there","world"],3]""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("there"))))
  }

  test("object literals match exactly") {
    (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1}""")) must equal (Some(Map.empty))
  }

  test("object literals match subset") {
    (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1,'gnu':3}""")) must equal (Some(Map.empty))
  }

  test("object literals do not match superset") {
    (j("""{'hello':1,'world':2}""") matches j("""{'world':2}""")) must equal (None)
  }

  test("object variables match") {
    val a = Variable.raw[JNumber]()
    (PObject("hello" -> 1, "there" -> a, "world" -> 3) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Some(Map(a -> JIntegral(2))))
  }

  test("nest variables match") {
    val a = Variable.raw[JNumber]()
    val b = Variable.raw[JString]()
    (PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b), "world" -> 3) matches j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("gnus"))))
  }

  test("variables look up results") {
    val a = Variable.raw[JNumber]()
    val results: Pattern.Results = Map(a -> JIntegral(5))
    a(results) must equal (JIntegral(5))
  }

  test("variables look up failure") {
    val a = Variable.raw[JValue]()
    val results: Pattern.Results = Map.empty
    evaluating { a(results) } must produce [NoSuchElementException]
  }

  test("variables can match the same thing twice") {
    val a = Variable.raw[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'happy'}""")) must equal (Some(Map(a -> JString("happy"))))
  }

  test("variables fail to match different things") {
    val a = Variable.raw[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'sad'}""")) must equal (None)
  }

  test("patterns can be matched") {
    val a = Variable.raw[JNumber]()
    val b = Variable.raw[JString]()
    val Pattern1 = Literal(JNull)
    val Pattern2 = PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b), "world" -> 3)
    val scrutinee = j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")
    scrutinee match {
      case Pattern1(results) =>
        fail("It should not have matched Pattern1")
      case Pattern2(results) =>
        a(results) must equal (JIntegral(2))
        b(results) must equal (JString("gnus"))
      case _ =>
        fail("It should have matched Pattern2")
    }
  }
}
