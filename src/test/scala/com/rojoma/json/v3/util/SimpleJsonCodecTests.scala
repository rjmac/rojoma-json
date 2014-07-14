package com.rojoma.json.v3
package util

import ast._
import codec._

import org.scalatest.{FunSuite, MustMatchers}

case class Foo(a: Int, b: Option[String])

class SimpleJsonCodecTests extends FunSuite with MustMatchers {
  implicit val codec = SimpleJsonCodecBuilder[Foo].build("a", _.a, "b", _.b)

  test("Generated codecs work") {
    JsonEncode.toJValue(Foo(1, Some("one"))) must equal (JObject(Map("a" -> JNumber(1), "b" -> JString("one"))))
    JsonUtil.parseJson[Foo]("{a:1,b:'two'}") must equal (Right(Foo(1, Some("two"))))
  }

  test("Nones serialize as nothing at all") {
    JsonEncode.toJValue(Foo(2, None)) must equal (JObject(Map("a" -> JNumber(2))))
    JsonUtil.parseJson[Foo]("{a:1}") must equal (Right(Foo(1, None)))
  }
}
