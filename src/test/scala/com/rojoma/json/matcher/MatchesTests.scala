package com.rojoma.json
package matcher

import ast._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class MatchesTests extends FunSuite with MustMatchers {
  def j(s: String) = io.JsonReader.fromString(s)

  locally {
    // tests which depend on having the JValue => Literal conversion in scope because
    // "matches" is used on a JValue directly.

    import OptPattern._

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

    test("sequence literals match exactly") {
      (j("""[1,2,3]""") matches j("""[1,2,3]""")) must equal (Some(Map.empty))
    }

    test("sequence literals do NOT match prefix") {
      (j("""[1,2,3]""") matches j("""[1,2,3,4,5]""")) must equal (None)
    }

    test("sequence literals do not match overlong") {
      (j("""[1,2,3]""") matches j("""[1,2]""")) must equal (None)
    }

    test("sequence literals to not match mismatch") {
      (j("""[1,2,3]""") matches j("""[1,3,3]""")) must equal (None)
    }

    test("sequence literals inside other literals match like outer ones") {
      (j("""[[1,2,3]]""") matches j("""[[1,2,3]]""")) must equal (Some(Map.empty))
      (j("""[[1,2,3]]""") matches j("""[[1,2,3,4,5]]""")) must equal (None)
      (j("""[[1,2,3]]""") matches j("""[[1,2]]""")) must equal (None)
      (j("""[[1,2,3]]""") matches j("""[[1,3,3]]""")) must equal (None)
    }

    test("object literals match exactly") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1}""")) must equal (Some(Map.empty))
    }

    test("object literals do not match subset") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1,'gnu':3}""")) must equal (None)
    }

    test("object literals do not match superset") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2}""")) must equal (None)
    }

    test("nested object literals match the same as top-level ones") {
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2,'hello':1}]""")) must equal (Some(Map.empty))
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2,'hello':1,'gnu':3}]""")) must equal (None)
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2}]""")) must equal (None)
    }
  }

  test("variables get filled in") {
    val x = Variable[JBoolean]()
    (x matches JBoolean(true)) must equal (Some(Map(x -> JBoolean(true))))
    (x matches JBoolean(false)) must equal (Some(Map(x -> JBoolean(false))))
  }

  test("variables don't match") {
    val x = Variable[JString]()
    (x matches JBoolean(true)) must equal (None)
  }

  test("sequence variables match") {
    val middle = Variable[JIntegral]()
    (PArray(1, middle, 3) matches j("""[1,2,3]""")) must equal (Some(Map(middle -> JIntegral(2))))
  }

  test("nested sequence variables match") {
    val a = Variable[JIntegral]()
    val b = Variable[JString]()
    (PArray(1, a, PArray("hello", b, "world"), 3) matches j("""[1,2,["hello","there","world"],3]""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("there"))))
  }

  test("object variables match") {
    val a = Variable[JNumber]()
    (PObject("hello" -> 1, "there" -> a, "world" -> 3) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Some(Map(a -> JIntegral(2))))
  }

  test("object variables can be optional") {
    val a = Variable[JNumber]()
    (PObject("hello" -> 1, "there" -> 2, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Some(Map(a -> JIntegral(3))))
    (PObject("hello" -> 1, "there" -> 2, "world" -> POption(a)) matches j("""{'hello':1,'there':2}""")) must equal (Some(Map.empty))
  }

  test("optional variables are bound only once") {
    val a = Variable[JNumber]()
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (None)
    (PObject("hello" -> 1, "there" -> a, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (None)
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (None)
    (PObject("hello" -> 1, "there" -> a, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':2}""")) must equal (Some(Map(a -> JIntegral(2))))
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'there':2,'world':2}""")) must equal (Some(Map(a -> JIntegral(2))))
  }

  test("omitted optional variables don't affect present ones") {
    val a = Variable[JNumber]()
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'world':2}""")) must equal (Some(Map(a -> JIntegral(2))))
  }

  test("optional fields will not match null") {
    (PObject("hello" -> POption(1)) matches j("""{'hello':null}""")) must equal (None)
  }

  test("optional fields will match null only if the subpattern doesn't acept it") {
    var a = Variable[JValue]
    (PObject("hello" -> POption(a)) matches j("""{'hello':null}""")) must equal (Some(Map(a -> JNull)))
  }

  test("nest variables match") {
    val a = Variable[JNumber]()
    val b = Variable[JString]()
    (PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b), "world" -> 3) matches j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")) must equal (Some(Map(a -> JIntegral(2), b -> JString("gnus"))))
  }

  test("variables look up results") {
    val a = Variable[JNumber]()
    val results: Pattern.Results = Map(a -> JIntegral(5))
    a(results) must equal (JIntegral(5))
  }

  test("variables look up failure") {
    val a = Variable[JValue]()
    val results: Pattern.Results = Map.empty
    evaluating { a(results) } must produce [NoSuchElementException]
  }

  test("variables can match the same thing twice") {
    val a = Variable[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'happy'}""")) must equal (Some(Map(a -> JString("happy"))))
  }

  test("variables fail to match different things") {
    val a = Variable[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'sad'}""")) must equal (None)
  }

  test("codecs can be matched") {
    val a = Variable[List[String]]()
    (PObject("hello" -> a) matches j("""{'hello':['happy','there','sad']}""")) must equal (Some(Map(a -> List("happy","there","sad"))))
  }

  test("types with codecs can be matched as literals") {
    (PObject("hello" -> List("happy","there","sad")) matches j("""{'hello':['happy','there','sad']}""")) must equal (Some(Map.empty))
    (PObject("hello" -> List("happy")) matches j("""{'hello':['happy','there','sad']}""")) must equal (None)
    (PObject("hello" -> "world") matches j("""{'hello':'world'}""")) must equal (Some(Map.empty))
  }

  test("codecs match normally") {
    val a = Variable[String]()
    (PObject("hello" -> a, "world" -> a) matches j("""{'hello':'happy','world':'gnu'}""")) must equal (None)
    (PObject("hello" -> a, "world" -> a) matches j("""{'hello':'happy','world':'happy'}""")) must equal (Some(Map(a -> "happy")))
  }

  test("switch matches the first possibility") {
    val a = Variable[JString]()
    val b = Variable[JBoolean]()
    (FirstOf(a, b) matches JString("hello")) must equal (Some(Map(a -> JString("hello"))))
    (FirstOf(a, b) matches JBoolean(true)) must equal (Some(Map(b -> JBoolean(true))))
  }

  test("patterns can be matched") {
    val a = Variable[JNumber]()
    val b = Variable[JString]()
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
