package com.rojoma.json
package util

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

import ast._
import io._
import codec._

object SimpleHierarchyCodecBuilderTests {
  // A simple hierarchy
  sealed abstract class Base
  case class A(x: String) extends Base
  implicit val aCodec = SimpleJsonCodecBuilder[A].gen("x", _.x)
  case class B(x: Int) extends Base
  implicit val bCodec = com.rojoma.json.util.SimpleJsonCodecBuilder[B].gen("x", _.x)

  def externalBaseCodec =
    SimpleHierarchyCodecBuilder[Base](SimpleHierarchyCodecBuilder.External).
      branch[A]("a").
      branch[B]("b").
      gen

  def internalBaseCodec =
    SimpleHierarchyCodecBuilder[Base](SimpleHierarchyCodecBuilder.Internal("type")).
      branch[A]("a").
      branch[B]("b").
      gen

  def j(s: String) = JsonReader.fromString(s)
}

class SimpleHierarchyCodecBuilderTests extends FunSuite with MustMatchers {
  import SimpleHierarchyCodecBuilderTests._

  test("Can encode a simple hierarchy with the external type") {
    val baseCodec = externalBaseCodec
    baseCodec.encode(A("hello")) must equal (j("{a : {x : 'hello'}}"))
    baseCodec.encode(B(5)) must equal (j("{b : {x : 5}}"))
  }

  test("Can decode a simple hierarchy with the external type") {
    val baseCodec = externalBaseCodec
    baseCodec.decode(j("{a : {x : 'hello'}}")) must equal (Some(A("hello")))
    baseCodec.decode(j("{b : {x : 5}}")) must equal (Some(B(5)))
  }

  test("No tag-match returns None for external") {
    val baseCodec = externalBaseCodec
    baseCodec.decode(j("{c : 'whatever'}")) must equal (None)
  }

  test("Bad inner value returns None for external") {
    val baseCodec = externalBaseCodec
    baseCodec.decode(j("{a : 'whatever'}")) must equal (None)
  }

  test("Can encode a simple hierarchy with the internal type") {
    val baseCodec = internalBaseCodec
    baseCodec.encode(A("hello")) must equal (j("{type : 'a', x : 'hello'}"))
    baseCodec.encode(B(5)) must equal (j("{type : 'b', x : 5}"))
  }

  test("Can decode a simple hierarchy with the internal type") {
    val baseCodec = internalBaseCodec
    baseCodec.decode(j("{type : 'a', x : 'hello'}")) must equal (Some(A("hello")))
    baseCodec.decode(j("{type : 'b', x : 5}")) must equal (Some(B(5)))
  }

  test("No tag-match returns None for internal") {
    val baseCodec = internalBaseCodec
    baseCodec.decode(j("{type : 'c', 'value' : 'whatever'}")) must equal (None)
  }

  test("Bad branch value returns None for internal") {
    val baseCodec = internalBaseCodec
    baseCodec.decode(j("{type : 'a'}")) must equal (None)
  }

  test("Trying to give the same name to two branches fails") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](SimpleHierarchyCodecBuilder.External).
        branch[A]("a").
        branch[B]("a")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same type two different names fails") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](SimpleHierarchyCodecBuilder.External).
        branch[A]("a").
        branch[A]("b")
    } must produce [IllegalArgumentException]
  }

  test("Defining no branches fails") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](SimpleHierarchyCodecBuilder.External).gen
    } must produce [IllegalStateException]
  }
}
