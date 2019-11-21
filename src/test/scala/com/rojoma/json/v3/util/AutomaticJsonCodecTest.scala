package com.rojoma.json.v3
package util

import org.scalatest.{FunSuite, MustMatchers, EitherValues}

import ast._
import codec._
import interpolation._

class AutomaticJsonCodecTest extends FunSuite with MustMatchers with EitherValues {
  test("Automatic codecs via annotation work without parameters") {
    @AutomaticJsonCodec case class Foo(x: Int, xs: Seq[Int])
    JsonEncode.toJValue(Foo(5, List(1,2,3))) must equal (json"""{x: 5, xs: [1,2,3]}""")
    JsonDecode.fromJValue[Foo](json"""{x: 5, xs: [1,2,3]}""") must equal (Right(Foo(5, List(1,2,3))))
  }

  test("Automatic codecs via annotation work with parameters") {
    @AutomaticJsonCodec case class Foo[Q](x: Int, xs: Seq[Q])
    JsonEncode.toJValue(Foo(5, List(1,2,3))) must equal (json"""{x: 5, xs: [1,2,3]}""")
    JsonDecode.fromJValue[Foo[Int]](json"""{x: 5, xs: [1,2,3]}""") must equal (Right(Foo(5, List(1,2,3))))
  }

  test("Automatic codecs via annotation work with parameters that have bounds") {
    @AutomaticJsonCodec case class Foo[Q <: String](x: Int, xs: Seq[Q])
    JsonEncode.toJValue(Foo(42, List("a","b","c"))) must equal (json"""{x: 42, xs: ["a","b","c"]}""")
    JsonDecode.fromJValue[Foo[String]](json"""{x: 42, xs: ["a","b","c"]}""") must equal (Right(Foo(42, List("a","b","c"))))
  }

  test("Automatic encodes via annotation work without parameters") {
    @AutomaticJsonEncode case class Foo(x: Int, xs: Seq[Int])
    JsonEncode.toJValue(Foo(5, List(1,2,3))) must equal (json"""{x: 5, xs: [1,2,3]}""")
  }

  test("Automatic encodes via annotation work with parameters") {
    @AutomaticJsonEncode case class Foo[Q](x: Int, xs: Seq[Q])
    JsonEncode.toJValue(Foo(5, List(1,2,3))) must equal (json"""{x: 5, xs: [1,2,3]}""")
  }

  test("Automatic encodes via annotation work with parameters that have bounds") {
    @AutomaticJsonEncode case class Foo[Q <: String](x: Int, xs: Seq[Q])
    JsonEncode.toJValue(Foo(42, List("a","b","c"))) must equal (json"""{x: 42, xs: ["a","b","c"]}""")
  }

  test("Automatic decodes via annotation work without parameters") {
    @AutomaticJsonDecode case class Foo(x: Int, xs: Seq[Int])
    JsonDecode.fromJValue[Foo](json"""{x: 5, xs: [1,2,3]}""") must equal (Right(Foo(5, List(1,2,3))))
  }

  test("Automatic decodes via annotation work with parameters") {
    @AutomaticJsonDecode case class Foo[Q](x: Int, xs: Seq[Q])
    JsonDecode.fromJValue[Foo[Int]](json"""{x: 5, xs: [1,2,3]}""") must equal (Right(Foo(5, List(1,2,3))))
  }

  test("Automatic decodes via annotation work with parameters that have bounds") {
    @AutomaticJsonDecode case class Foo[Q <: String](x: Int, xs: Seq[Q])
    JsonDecode.fromJValue[Foo[String]](json"""{x: 42, xs: ["a","b","c"]}""") must equal (Right(Foo(42, List("a","b","c"))))
  }
}
