package com.rojoma.json
package matcher

import ast._

import org.scalatest.FunSuite
import org.scalatest.Assertions

class MatchesTests extends FunSuite with Assertions {
  import Pattern._

  def j(s: String) = io.JsonReader.fromString(s)

  test("atom literals match") {
    assert(JNull matches JNull)
    assert(JBoolean(true) matches JBoolean(true))
    assert(JBoolean(false) matches JBoolean(false))
    assert(JString("hello") matches JString("hello"))
  }

  test("atom literals don't match") {
    assert(!(JNull matches JBoolean(true)))
    assert(!(JBoolean(true) matches JBoolean(false)))
    assert(!(JString("hello") matches JString("world")))
  }

  test("variables get filled in") {
    val x = Variable[JBoolean]
    assert(x matches JBoolean(true))
    assert(x.result.boolean)

    assert(x matches JBoolean(false))
    assert(!x.result.boolean)
  }

  test("variables don't match") {
    val x = Variable[JString]
    assert(!(x matches JBoolean(true)))
    assert(x.result eq null)
  }

  test("sequence literals match exactly") {
    assert(j("""[1,2,3]""") matches j("""[1,2,3]"""))
  }

  test("sequence literals match prefix") {
    assert(j("""[1,2,3]""") matches j("""[1,2,3,4,5]"""))
  }

  test("sequence literals do not match overlong") {
    assert(!(j("""[1,2,3]""") matches j("""[1,2]""")))
  }

  test("sequence literals to not match mismatch") {
    assert(!(j("""[1,2,3]""") matches j("""[1,3,3]""")))
  }

  test("sequence variables match") {
    val middle = Variable[JIntegral]
    assert(VArray(1, middle, 3) matches j("""[1,2,3]"""))
    assert(middle.result.integral === 2)
  }

  test("nested sequence variables match") {
    val a = Variable[JIntegral]
    val b = Variable[JString]
    assert(VArray(1, a, VArray("hello", b, "world"), 3) matches j("""[1,2,["hello","there","world"],3]"""))
    assert(a.result.integral === 2)
    assert(b.result.string === "there")
  }

  test("object literals match exactly") {
    assert(j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1}"""))
  }

  test("object literals match subset") {
    assert(j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1,'gnu':3}"""))
  }

  test("object literals do not match superset") {
    assert(!(j("""{'hello':1,'world':2}""") matches j("""{'world':2}""")))
  }

  test("object variables match") {
    val a = Variable[JNumber]
    assert(VObject("hello" -> 1, "there" -> a, "world" -> 3) matches j("""{'hello':1,'there':2,'world':3}"""))
    assert(a.result.integral === 2)
  }

  test("nest variables match") {
    val a = Variable[JNumber]
    val b = Variable[JString]
    assert(VObject("hello" -> 1, "there" -> a, "gnu" -> VObject("smiling" -> b), "world" -> 3) matches j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}"""))
    assert(a.result.integral === 2)
    assert(b.result.string === "gnus")
  }
}
