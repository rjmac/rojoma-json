package com.rojoma.json
package util

import ast._
import codec.JsonCodec.toJValue

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

case class Foo(a: Int, b: Option[String])

class SimpleJsonCodecTests extends FunSuite with MustMatchers {
  implicit val codec = SimpleJsonCodecBuilder[Foo].gen("a", _.a, "b", _.b)

  test("Generated codecs work") {
    toJValue(Foo(1, Some("one"))) must equal (JObject(Map("a" -> JNumber(1), "b" -> JString("one"))))
    JsonUtil.parseJson[Foo]("{a:1,b:'two'}") must equal (Some(Foo(1, Some("two"))))
  }

  test("Nones serialize as nothing at all") {
    toJValue(Foo(2, None)) must equal (JObject(Map("a" -> JNumber(2))))
    JsonUtil.parseJson[Foo]("{a:1}") must equal (Some(Foo(1, None)))
  }
}
