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
    val x = new Variable[JBoolean]
    (x matches JBoolean(true)) must equal (Some(Map(x -> JBoolean(true))))
    (x matches JBoolean(false)) must equal (Some(Map(x -> JBoolean(false))))
  }

  test("variables don't match") {
    val x = new Variable[JString]
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
    val middle = new Variable[JIntegral]
    (VArray(1, middle, 3) matches j("""[1,2,3]""")) must equal (Some(Map(middle -> JIntegral(2))))
  }

  test("nested sequence variables match") {
    val a = new Variable[JIntegral]
    val b = new Variable[JString]
    (VArray(1, a, VArray("hello", b, "world"), 3) matches j("""[1,2,["hello","there","world"],3]""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("there"))))
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
    val a = new Variable[JNumber]
    (VObject("hello" -> 1, "there" -> a, "world" -> 3) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Some(Map(a -> JIntegral(2))))
  }

  test("nest variables match") {
    val a = new Variable[JNumber]
    val b = new Variable[JString]
    (VObject("hello" -> 1, "there" -> a, "gnu" -> VObject("smiling" -> b), "world" -> 3) matches j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("gnus"))))
  }

  test("variables look up results") {
    val a = new Variable[JNumber]
    val results: Pattern.Results = Map(a -> JIntegral(5))
    a(results) must equal (JIntegral(5))
  }

  test("variables look up failure") {
    val a = new Variable[JValue]
    val results: Pattern.Results = Map.empty
    evaluating { a(results) } must produce [NoSuchElementException]
  }

  test("patterns can be matched") {
    val a = new Variable[JNumber]
    val b = new Variable[JString]
    val Pattern1 = Literal(JNull)
    val Pattern2 = VObject("hello" -> 1, "there" -> a, "gnu" -> VObject("smiling" -> b), "world" -> 3)
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
