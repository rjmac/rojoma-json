package com.rojoma.json.v3
package matcher

import scala.language.implicitConversions

import ast._
import codec._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class MatchesTests extends AnyFunSuite with Matchers {
  def j(s: String) = io.JsonReader.fromString(s)

  locally {
    // tests which depend on having the JValue => Literal conversion in scope because
    // "matches" is used on a JValue directly.

    import OptPattern.given

    test("atom literals match") {
      (JNull matches JNull) must equal (Right(Map.empty))
      (JBoolean(true) matches JBoolean(true)) must equal (Right(Map.empty))
      (JBoolean(false) matches JBoolean(false)) must equal (Right(Map.empty))
      (JString("hello") matches JString("hello")) must equal (Right(Map.empty))
    }

    test("atom literals don't match") {
      (JNull matches JBoolean(true)) must equal (Left(DecodeError.InvalidType(JNull, JBoolean, Path.empty)))
      (JBoolean(true) matches JBoolean(false)) must equal (Left(DecodeError.InvalidValue(JBoolean(false), Path.empty)))
      (JString("hello") matches JString("world")) must equal (Left(DecodeError.InvalidValue(JString("world"), Path.empty)))
    }

    test("sequence literals match exactly") {
      (j("""[1,2,3]""") matches j("""[1,2,3]""")) must equal (Right(Map.empty))
    }

    test("sequence literals do NOT match prefix") {
      (j("""[1,2,3]""") matches j("""[1,2,3,4,5]""")) must equal (Left(DecodeError.InvalidLength(3, 5, Path.empty)))
    }

    test("sequence literals do not match overlong") {
      (j("""[1,2,3]""") matches j("""[1,2]""")) must equal (Left(DecodeError.InvalidLength(3, 2, Path.empty)))
    }

    test("sequence literals do not match mismatch") {
      (j("""[1,2,3]""") matches j("""[1,3,3]""")) must equal (Left(DecodeError.InvalidValue(j("""3"""), Path(1))))
    }

    test("sequence literals inside other literals match like outer ones") {
      (j("""[[1,2,3]]""") matches j("""[[1,2,3]]""")) must equal (Right(Map.empty))
      (j("""[[1,2,3]]""") matches j("""[[1,2,3,4,5]]""")) must equal (Left(DecodeError.InvalidLength(3, 5, Path(0))))
      (j("""[[1,2,3]]""") matches j("""[[1,2]]""")) must equal (Left(DecodeError.InvalidLength(3, 2, Path(0))))
      (j("""[[1,2,3]]""") matches j("""[[1,3,3]]""")) must equal (Left(DecodeError.InvalidValue(j("""3"""), Path(0, 1))))
    }

    test("object literals match exactly") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1}""")) must equal (Right(Map.empty))
    }

    test("object literals do match superset") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2,'hello':1,'gnu':3}""")) must equal (Right(Map.empty))
    }

    test("object literals do not match subset") {
      (j("""{'hello':1,'world':2}""") matches j("""{'world':2}""")) must equal (Left(DecodeError.MissingField("hello", Path.empty)))
    }

    test("nested object literals match the same as top-level ones") {
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2,'hello':1}]""")) must equal (Right(Map.empty))
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2,'hello':1,'gnu':3}]""")) must equal (Right(Map.empty))
      (j("""[{'hello':1,'world':2}]""") matches j("""[{'world':2}]""")) must equal (Left(DecodeError.MissingField("hello", Path(0))))
    }
  }

  test("variables get filled in") {
    val x = Variable[JBoolean]()
    (x matches JBoolean(true)) must equal (Right(Map(x -> JBoolean(true))))
    (x matches JBoolean(false)) must equal (Right(Map(x -> JBoolean(false))))
  }

  test("variables don't match") {
    val x = Variable[JString]()
    (x matches JBoolean(true)) must equal (Left(DecodeError.InvalidType(JString, JBoolean, Path.empty)))
  }

  test("sequence variables match") {
    val middle = Variable[Int]()
    (PArray(1, middle, 3) matches j("""[1,2,3]""")) must equal (Right(Map(middle -> 2)))
  }

  test("nested sequence variables match") {
    val a = Variable[Int]()
    val b = Variable[String]()
    (PArray(1, a, PArray("hello", b, "world"), 3) matches j("""[1,2,["hello","there","world"],3]""")) must equal (Right(Map(a -> 2, b -> "there")))
  }

  test("object variables match") {
    val a = Variable[Int]()
    (PObject("hello" -> 1, "there" -> a, "world" -> 3) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Right(Map(a -> 2)))
  }

  test("object variables can be optional") {
    val a = Variable[Int]()
    (PObject("hello" -> 1, "there" -> 2, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Right(Map(a -> 3)))
    (PObject("hello" -> 1, "there" -> 2, "world" -> POption(a)) matches j("""{'hello':1,'there':2}""")) must equal (Right(Map.empty))
  }

  test("optional variables are bound only once") {
    val a = Variable[Int]()
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Left(DecodeError.InvalidValue(JNumber(3), Path("world"))))
    (PObject("hello" -> 1, "there" -> a, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Left(DecodeError.InvalidValue(JNumber(3), Path("world"))))
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'there':2,'world':3}""")) must equal (Left(DecodeError.InvalidValue(JNumber(3), Path("world"))))
    (PObject("hello" -> 1, "there" -> a, "world" -> POption(a)) matches j("""{'hello':1,'there':2,'world':2}""")) must equal (Right(Map(a -> 2)))
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'there':2,'world':2}""")) must equal (Right(Map(a -> 2)))
  }

  test("omitted optional variables don't affect present ones") {
    val a = Variable[Int]()
    (PObject("hello" -> 1, "there" -> POption(a), "world" -> a) matches j("""{'hello':1,'world':2}""")) must equal (Right(Map(a -> 2)))
  }

  test("optional fields will not match null") {
    (PObject("hello" -> POption(1)) matches j("""{'hello':null}""")) must equal (Left(DecodeError.InvalidType(JNumber, JNull, Path("hello"))))
  }

  test("optional fields will match null if the subpattern does accept it") {
    val a = Variable[JValue]()
    (PObject("hello" -> POption(a)) matches j("""{'hello':null}""")) must equal (Right(Map(a -> JNull)))
  }

  test("optional fields reject if present but unmatching") {
    val a = Variable[JString]()
    (PObject("hello" -> POption(a)) matches j("""{'hello':5}""")) must equal (Left(DecodeError.InvalidType(JString, JNumber, Path("hello"))))
  }

  test("optional-or-null fields will match null") {
    val a = Variable[Int]()
    (PObject("hello" -> POption(a).orNull) matches j("""{'hello':null}""")) must equal (Right(Map.empty))
  }

  test("optional-or-null fields will match if present") {
    val a = Variable[Int]()
    (PObject("hello" -> POption(a).orNull) matches j("""{'hello':1}""")) must equal (Right(Map(a -> 1)))
  }

  test("optional-or-null fields will reject if present, non-null, and not-matching") {
    val a = Variable[Int]()
    (PObject("hello" -> POption(a).orNull) matches j("""{'hello':'world'}""")) must equal (Left(DecodeError.Multiple(List(DecodeError.InvalidType(JNumber, JString, Path("hello")), DecodeError.InvalidType(JNull, JString, Path("hello"))))))
  }

  test("nest variables match") {
    val a = Variable[Int]()
    val b = Variable[String]()
    (PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b), "world" -> 3) matches j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")) must equal (Right(Map(a -> 2, b -> "gnus")))
  }

  test("variables look up results") {
    val a = Variable[Int]()
    val results: Pattern.Results = Map(a -> 5)
    a(results) must equal (5)
  }

  test("variables look up failure") {
    val v = Variable[JValue]()
    val results: Pattern.Results = Map.empty
    a [NoSuchElementException] must be thrownBy { v(results) }
  }

  test("variables can match the same thing twice") {
    val a = Variable[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'happy'}""")) must equal (Right(Map(a -> JString("happy"))))
  }

  test("variables fail to match different things") {
    val a = Variable[JValue]()
    (PObject("hello" -> a, "there" -> a) matches j("""{'hello':'happy','there':'sad'}""")) must equal (Left(DecodeError.InvalidValue(JString("sad"), Path("there"))))
  }

  test("codecs can be matched") {
    val a = Variable[List[String]]()
    (PObject("hello" -> a) matches j("""{'hello':['happy','there','sad']}""")) must equal (Right(Map(a -> List("happy","there","sad"))))
  }

  test("types with codecs can be matched as literals") {
    // I'm not sure why scala 3 doesn't pick up that List[String] has
    // JsonEncode and JsonDecodes anymore, while it does these other
    // things?  It probably has something to do with the givens being
    // defined over any subclass of Seq?
    (PObject("hello" -> java.util.Arrays.asList("happy","there","sad")) matches j("""{'hello':['happy','there','sad']}""")) must equal (Right(Map.empty))
    (PObject("hello" -> java.util.Arrays.asList("happy")) matches j("""{'hello':['happy','there','sad']}""")) must equal (Left(DecodeError.InvalidValue(j("""['happy','there','sad']"""), Path("hello"))))
    (PObject("hello" -> "world") matches j("""{'hello':'world'}""")) must equal (Right(Map.empty))
  }

  test("codecs match normally") {
    val a = Variable[String]()
    (PObject("hello" -> a, "world" -> a) matches j("""{'hello':'happy','world':'gnu'}""")) must equal (Left(DecodeError.InvalidValue(JString("gnu"), Path("world"))))
    (PObject("hello" -> a, "world" -> a) matches j("""{'hello':'happy','world':'happy'}""")) must equal (Right(Map(a -> "happy")))
  }

  test("switch matches the first possibility") {
    val a = Variable[JString]()
    val b = Variable[JBoolean]()
    (FirstOf(a, b) matches JString("hello")) must equal (Right(Map(a -> JString("hello"))))
    (FirstOf(a, b) matches JBoolean(true)) must equal (Right(Map(b -> JBoolean(true))))
  }

  test("patterns can be matched") {
    val a = Variable[Int]()
    val b = Variable[String]()
    val Pattern1 = Literal(JNull)
    val Pattern2 = PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b), "world" -> 3)
    val scrutinee = j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}""")
    scrutinee match {
      case Pattern1(results) =>
        fail("It should not have matched Pattern1")
      case Pattern2(results) =>
        a(results) must equal (2)
        b(results) must equal ("gnus")
      case _ =>
        fail("It should have matched Pattern2")
    }
  }

  test("patterns can generate JSON") {
    val a = Variable[Int]()
    val b = Variable[String]()
    val pattern = PObject("hello" -> 1, "there" -> a, "gnu" -> PObject("smiling" -> b, "are" -> "happy"), "world" -> 3)
    pattern.generate(a := 2, b := "gnus") must equal (j("""{'hello':1,'there':2,'world':3,'gnu':{'smiling':'gnus','are':'happy'}}"""))
  }

  test("optional fields that fail to generate provide nothing") {
    val a = Variable[Int]()
    val b = Variable[String]()
    val pattern = PObject("hello" -> POption(a), "there" -> b)
    pattern.generate(b := "gnus") must equal (j("""{'there':'gnus'}"""))
  }

  test("non-optional fields throw an exception") {
    val v = Variable[Int]()
    val pattern = PObject("hello" -> v)
    a [JsonGenerationException] must be thrownBy { pattern.generate() }
  }

  test("generating from AllOf throws an exception") {
    val pattern = AllOf("hello", "world")
    a [JsonGenerationException] must be thrownBy { pattern.generate() }
  }

  test("generating from FirstOf produces the first option that can be generated") {
    val a = Variable[String]()
    val pattern = FirstOf(a, "hello", "world")
    pattern.generate() must equal (JString("hello"))
  }

  test("Optional pattern-or-null produces nothing if the pattern doesn't generate") {
    val a = Variable[String]()
    val pattern = PObject("hello" -> POption(FirstOf(a, JNull)))
    pattern.generate() must equal (j("""{}"""))
  }

  test(":=? works") {
    val a = Variable[String]()
    val b = Variable[Int]()
    val pattern = FirstOf(a, b)
    pattern.generate(a :=? None, b := 5) must equal (JNumber(5))
    pattern.generate(a :=? Some("gnu"), b := 5) must equal (JString("gnu"))
  }
}
