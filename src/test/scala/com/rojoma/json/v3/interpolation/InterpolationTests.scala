package com.rojoma.json.v3.interpolation

import java.net.URI
import java.util.UUID

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import com.rojoma.json.v3.ast._

class InterpolationTests extends FunSuite with MustMatchers {
  test("Empty arrays are parsed") {
    json"""[]""" must be theSameInstanceAs (JArray.canonicalEmpty)
  }

  test("Empty objects are parsed") {
    json"""{}""" must be theSameInstanceAs (JObject.canonicalEmpty)
  }

  test("Booleans are parsed") {
    json"""true""" must be theSameInstanceAs (JBoolean.canonicalTrue)
    json"""false""" must be theSameInstanceAs (JBoolean.canonicalFalse)
  }


  test("Optional array item") {
    json"""[?${Some(2)}]""" must be (json"""[2]""")
    json"""[1, ?${Some(2)}]""" must be (json"""[1, 2]""")
    json"""[?${Some(2)}, 3]""" must be (json"""[2, 3]""")
    json"""[1, ?${Some(2)}, 3]""" must be (json"""[1, 2, 3]""")
    json"""[?${Option.empty[Int]}]""" must be (json"""[]""")
    json"""[?${Option.empty[Int]}, 3]""" must be (json"""[3]""")
    json"""[1,?${Option.empty[Int]}]""" must be (json"""[1]""")
    json"""[1,?${Option.empty[Int]}, 3]""" must be (json"""[1, 3]""")
  }

  test("Optional map item") {
    json"""{b : ?${Some(2)}}""" must be (json"""{b:2}""")
    json"""{a:1, b: ?${Some(2)}}""" must be (json"""{a:1, b:2}""")
    json"""{b:?${Some(2)}, c:3}""" must be (json"""{b:2, c:3}""")
    json"""{a:1, b:?${Some(2)}, c:3}""" must be (json"""{a:1, b:2, c:3}""")
    json"""{b:?${Option.empty[Int]}}""" must be (json"""{}""")
    json"""{b:?${Option.empty[Int]}, c:3}""" must be (json"""{c:3}""")
    json"""{a:1,b:?${Option.empty[Int]}}""" must be (json"""{a:1}""")
    json"""{a:1,b:?${Option.empty[Int]}, c:3}""" must be (json"""{a:1, c:3}""")
  }

  test("Workout") {
    val interpolatedMap = Map(new URI("https://www.example.com/one") -> "smiling",
                              new URI("https://www.example.com/two") -> "gnus",
                              new URI("https://www.example.com/three") -> "are",
                              new URI("https://www.example.com/four") -> "happy")

    val interpolatedList = List(5,4,3,2,1)
    val interpolatedItem = "x"

    val interpolatedEmptyMap = Map.empty[UUID, Double]
    val interpolatedEmptyList = List.empty[String]

    val r = json"""{
      a : 1,
      b : true,
      c : $interpolatedList,
      d : ["x", ..$interpolatedList, "y", $interpolatedEmptyList, "z"],
      e : ["x", $interpolatedList, "y", ..$interpolatedEmptyList, "z"],
      f: $interpolatedMap,
      g: $interpolatedItem,
      h: $interpolatedEmptyMap,
      i: $interpolatedEmptyList,
      j: null,
      ..$interpolatedMap,
      ..$interpolatedEmptyMap
    }"""

    r must be (JObject(Map("a" -> JNumber(1),
                           "b" -> JBoolean(true),
                           "c" -> JArray(Seq(5,4,3,2,1).map(JNumber(_))),
                           "d" -> JArray(Seq(JString("x")) ++ Seq(5,4,3,2,1).map(JNumber(_)) ++ Seq(JString("y"), JArray.canonicalEmpty, JString("z"))),
                           "e" -> JArray(Seq(JString("x"), JArray(Seq(5,4,3,2,1).map(JNumber(_))), JString("y"), JString("z"))),
                           "f" -> JObject(Map("https://www.example.com/one" -> JString("smiling"),
                                              "https://www.example.com/two" -> JString("gnus"),
                                              "https://www.example.com/three" -> JString("are"),
                                              "https://www.example.com/four" -> JString("happy"))),
                           "g" -> JString("x"),
                           "h" -> JObject.canonicalEmpty,
                           "i" -> JArray.canonicalEmpty,
                           "j" -> JNull,
                           "https://www.example.com/one" -> JString("smiling"),
                           "https://www.example.com/two" -> JString("gnus"),
                           "https://www.example.com/three" -> JString("are"),
                           "https://www.example.com/four" -> JString("happy"))))
  }
}
