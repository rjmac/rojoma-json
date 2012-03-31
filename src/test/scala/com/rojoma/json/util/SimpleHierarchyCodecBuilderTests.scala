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

  def baseCodec(typeTag: TagType) =
    SimpleHierarchyCodecBuilder[Base](typeTag).
      branch[A]("a").
      branch[B]("b").
      gen

  def baseCodec(typeTag: NoTag) =
    SimpleHierarchyCodecBuilder[Base](typeTag).
      branch[A].
      branch[B].
      gen

  def j(s: String) = JsonReader.fromString(s)
}

class SimpleHierarchyCodecBuilderTests extends FunSuite with MustMatchers {
  import SimpleHierarchyCodecBuilderTests._

  test("Can encode a simple hierarchy with the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.encode(A("hello")) must equal (j("{type : 'a', value : {x : 'hello'}}"))
    bc.encode(B(5)) must equal (j("{type: 'b', value : {x : 5}}"))
  }

  test("Can encode a simple hierarchy with the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.encode(A("hello")) must equal (j("{'a' : {x : 'hello'}}"))
    bc.encode(B(5)) must equal (j("{'b' : {x : 5}}"))
  }

  test("Can encode a simple hierarchy with the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.encode(A("hello")) must equal (j("{type : 'a', x : 'hello'}"))
    bc.encode(B(5)) must equal (j("{type : 'b', x : 5}"))
  }

  test("Can encode a simple hierarchy with the no-tag type") {
    val bc = baseCodec(NoTag)
    bc.encode(A("hello")) must equal (j("{x : 'hello'}"))
    bc.encode(B(5)) must equal (j("{x : 5}"))
  }

  test("Can decode a simple hierarchy with the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.decode(j("{type : 'a', value : {x : 'hello'}}")) must equal (Some(A("hello")))
    bc.decode(j("{type : 'b', value : {x : 5}}")) must equal (Some(B(5)))
  }

  test("Can decode a simple hierarchy with the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{'a' : {x : 'hello'}}")) must equal (Some(A("hello")))
    bc.decode(j("{'b' : {x : 5}}")) must equal (Some(B(5)))
  }

  test("Can decode a simple hierarchy with the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'a', x : 'hello'}")) must equal (Some(A("hello")))
    bc.decode(j("{type : 'b', x : 5}")) must equal (Some(B(5)))
  }

  test("Can decode a simple hierarchy with the no-tag type") {
    val bc = baseCodec(NoTag)
    bc.decode(j("{x : 'hello'}")) must equal (Some(A("hello")))
    bc.decode(j("{x : 5}")) must equal (Some(B(5)))
  }

  test("No tag-match returns None for the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.decode(j("{'type' : 'whatever', value : 'haha'}")) must equal (None)
  }

  test("No tag-match returns None for the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{c : 'whatever'}")) must equal (None)
  }

  test("No tag-match returns None for the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'c', 'value' : 'whatever'}")) must equal (None)
  }

  test("Bad inner value returns None for the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.decode(j("{type: 'a', value : 'whatever'}")) must equal (None)
  }

  test("Bad inner value returns None for the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{a : 'whatever'}")) must equal (None)
  }

  test("Bad value value returns None for the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'a'}")) must equal (None)
  }

  test("Bad value value returns None for the no-tag type") {
    val bc = baseCodec(NoTag)
    bc.decode(j("'gnu'")) must equal (None)
  }

  test("Trying to use the same name for both the type and value fields fails") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("a", "a"))
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same name to two branches fails for tag-and-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).
        branch[A]("a").
        branch[B]("a")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same name to two branches fails for tag-to-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagToValue).
        branch[A]("a").
        branch[B]("a")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same name to two branches fails for internal-tag") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](InternalTag("t")).
        branch[A]("a").
        branch[B]("a")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same type two different names fails for tag-and-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).
        branch[A]("a").
        branch[A]("b")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same type two different names fails for tag-to-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagToValue).
        branch[A]("a").
        branch[A]("b")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same type two different names fails for internal-tag") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](InternalTag("t")).
        branch[A]("a").
        branch[A]("b")
    } must produce [IllegalArgumentException]
  }

  test("Trying to give the same type two different names fails for no-tag") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](NoTag).
        branch[A].
        branch[A]
    } must produce [IllegalArgumentException]
  }

  test("Defining no branches fails for tag-and-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).gen
    } must produce [IllegalStateException]
  }

  test("Defining no branches fails for tag-to-value") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](TagToValue).gen
    } must produce [IllegalStateException]
  }

  test("Defining no branches fails for internal-tag") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](InternalTag("x")).gen
    } must produce [IllegalStateException]
  }

  test("Defining no branches fails for no-tag") {
    evaluating {
      SimpleHierarchyCodecBuilder[Base](NoTag).gen
    } must produce [IllegalStateException]
  }
}
