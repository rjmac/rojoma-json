package com.rojoma.json.v3
package util

import org.scalatest.{FunSuite, MustMatchers}

import ast._
import io._
import codec._

object SimpleHierarchyCodecBuilderTests {
  // A simple hierarchy
  sealed abstract class Base
  case class A(x: String) extends Base
  implicit val aCodec = AutomaticJsonCodecBuilder[A]
  case class B(x: Int) extends Base
  implicit val bCodec = AutomaticJsonCodecBuilder[B]

  def baseCodec(typeTag: TagType) =
    SimpleHierarchyCodecBuilder[Base](typeTag).
      branch[A]("a").
      branch[B]("b").
      build

  def baseCodec(typeTag: NoTag) =
    SimpleHierarchyCodecBuilder[Base](typeTag).
      branch[A].
      branch[B].
      build

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
    bc.decode(j("{type : 'a', value : {x : 'hello'}}")) must equal (Right(A("hello")))
    bc.decode(j("{type : 'b', value : {x : 5}}")) must equal (Right(B(5)))
  }

  test("Can decode a simple hierarchy with the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{'a' : {x : 'hello'}}")) must equal (Right(A("hello")))
    bc.decode(j("{'b' : {x : 5}}")) must equal (Right(B(5)))
  }

  test("Can decode a simple hierarchy with the tag-to-value type even with random extra fields") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{'a' : {x : 'hello'}, 'c' : 'gnu'}")) must equal (Right(A("hello")))
    bc.decode(j("{'c' : 'gnu', 'b' : {x : 5}}")) must equal (Right(B(5)))
  }

  test("Can decode a simple hierarchy with the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'a', x : 'hello'}")) must equal (Right(A("hello")))
    bc.decode(j("{type : 'b', x : 5}")) must equal (Right(B(5)))
  }

  test("Can decode a simple hierarchy with the internal-tag type, without removing the tag") {
    val bc = baseCodec(InternalTag("type", removeTagForSubcodec = false))
    bc.decode(j("{type : 'a', x : 'hello'}")) must equal (Right(A("hello")))
    bc.decode(j("{type : 'b', x : 5}")) must equal (Right(B(5)))
  }

  test("Can decode a simple hierarchy with the no-tag type") {
    val bc = baseCodec(NoTag)
    bc.decode(j("{x : 'hello'}")) must equal (Right(A("hello")))
    bc.decode(j("{x : 5}")) must equal (Right(B(5)))
  }

  test("No tag-match returns Left for the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.decode(j("{'type' : 'whatever', value : 'haha'}")) must equal (Left(DecodeError.InvalidValue(JString("whatever"), Path.empty)))
  }

  test("No tag-match returns Left for the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{c : 'whatever'}")) must equal (Left(DecodeError.Multiple(List(DecodeError.MissingField("a", Path.empty), DecodeError.MissingField("b", Path.empty)))))
  }

  test("No tag-match returns Left for the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'c', 'value' : 'whatever'}")) must equal (Left(DecodeError.InvalidValue(JString("c"), Path.empty)))
  }

  test("Bad inner value returns Left for the tag-and-value type") {
    val bc = baseCodec(TagAndValue("type", "value"))
    bc.decode(j("{type: 'a', value : 'whatever'}")) must equal (Left(DecodeError.InvalidType(JObject, JString, new Path(List(Path.Field("value"))))))
  }

  test("Bad inner value returns Left for the tag-to-value type") {
    val bc = baseCodec(TagToValue)
    bc.decode(j("{a : 'whatever'}")) must equal (Left(DecodeError.InvalidType(JObject, JString, new Path(List(Path.Field("a"))))))
  }

  test("Bad value value returns Left for the internal-tag type") {
    val bc = baseCodec(InternalTag("type"))
    bc.decode(j("{type : 'a'}")) must equal (Left(DecodeError.MissingField("x", Path.empty)))
  }

  test("Bad value value returns Left for the no-tag type") {
    val bc = baseCodec(NoTag)
    bc.decode(j("'gnu'")) must equal (Left(DecodeError.InvalidType(JObject, JString, Path.empty)))
  }

  test("Trying to use the same name for both the type and value fields fails") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("a", "a"))
    }
  }

  test("Trying to give the same name to two branches fails for tag-and-value") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).
        branch[A]("a").
        branch[B]("a")
    }
  }

  test("Trying to give the same name to two branches fails for tag-to-value") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagToValue).
        branch[A]("a").
        branch[B]("a")
    }
  }

  test("Trying to give the same name to two branches fails for internal-tag") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](InternalTag("t")).
        branch[A]("a").
        branch[B]("a")
    }
  }

  test("Trying to give the same type two different names fails for tag-and-value") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).
        branch[A]("a").
        branch[A]("b")
    }
  }

  test("Trying to give the same type two different names fails for tag-to-value") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagToValue).
        branch[A]("a").
        branch[A]("b")
    }
  }

  test("Trying to give the same type two different names fails for internal-tag") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](InternalTag("t")).
        branch[A]("a").
        branch[A]("b")
    }
  }

  test("Trying to give the same type two different names fails for no-tag") {
    an [IllegalArgumentException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](NoTag).
        branch[A].
        branch[A]
    }
  }

  test("Defining no branches fails for tag-and-value") {
    an [IllegalStateException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).build
    }
  }

  test("Defining no branches fails for tag-to-value") {
    an [IllegalStateException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](TagToValue).build
    }
  }

  test("Defining no branches fails for internal-tag") {
    an [IllegalStateException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](InternalTag("x")).build
    }
  }

  test("Defining no branches fails for no-tag") {
    an [IllegalStateException] must be thrownBy {
      SimpleHierarchyCodecBuilder[Base](NoTag).build
    }
  }
}
