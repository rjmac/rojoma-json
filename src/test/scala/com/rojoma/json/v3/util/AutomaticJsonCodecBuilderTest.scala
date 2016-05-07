package com.rojoma.json.v3
package util

import org.scalatest.{FunSuite, MustMatchers, EitherValues}

import ast._
import codec._

class AutomaticJsonCodecBuilderTest extends FunSuite with MustMatchers with EitherValues {
  test("Undecorated Options with value None encode as nothing") {
    case class O(x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.renderJson(O(None)) must equal ("""{}""")
  }

  test("Undecorated Options with value Some encode as the thing") {
    case class O(x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.renderJson(O(Some(5))) must equal ("""{"x":5}""")
  }

  test("Options decorated with @NullForNone encode as `null' when None") {
    case class O(@NullForNone x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.renderJson(O(None)) must equal ("""{"x":null}""")
  }

  test("Options decorated with @NullForNone encode as the thing when Some") {
    case class O(@NullForNone x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.renderJson(O(Some(5))) must equal ("""{"x":5}""")
  }

  test("Undecorated Options decode as None when nothing is there") {
    case class O(x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{}").right.value must equal (O(None))
  }

  test("Undecorated Options decode as None when null is there") {
    case class O(x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{x:null}").right.value must equal (O(None))
  }

  test("Options decorated with @NullForNone decode as None when nothing is there") {
    case class O(@NullForNone x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{}").right.value must equal (O(None))
  }

  test("Options decorated with @NullForNone decode as None when null is there") {
    case class O(x: Option[Int])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{x:null}").right.value must equal (O(None))
  }

  test("`null' resolves to JNull when appropriate, even when wrapped in an option") {
    case class O(x: Option[JNull])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{x:null}").right.value must equal (O(Some(JNull)))
  }

  test("`null' resolves to JNull when appropriate, even when wrapped in an option and @NullForNone is set") {
    // this is a stupid thing to do, but the behaviour is nonetheless defined.
    case class O(@NullForNone x: Option[JNull])
    implicit val codec = AutomaticJsonCodecBuilder[O]
    JsonUtil.parseJson[O]("{x:null}").right.value must equal (O(Some(JNull)))
  }

  test("Recursive encodes properly") {
    case class Recursive(x: String, @LazyCodec xs: List[Recursive])
    implicit lazy val codec: JsonEncode[Recursive] with JsonDecode[Recursive] = AutomaticJsonCodecBuilder[Recursive]
    JsonUtil.renderJson(Recursive("b",List(Recursive("a",Nil), Recursive("c",Nil)))) must equal ("""{"x":"b","xs":[{"x":"a","xs":[]},{"x":"c","xs":[]}]}""")
  }

  test("By default keys are preserved") {
    case class DefaultKeys(helloWorld: String)
    implicit val codec = AutomaticJsonCodecBuilder[DefaultKeys]
    JsonUtil.renderJson(DefaultKeys("hello")) must equal ("""{"helloWorld":"hello"}""")
  }

  test("Keys are preserved under the Identity naming strategy") {
    @JsonKeyStrategy(Strategy.Identity)
    case class IdentityKeys(helloWorld: String)
    implicit val codec = AutomaticJsonCodecBuilder[IdentityKeys]
    JsonUtil.renderJson(IdentityKeys("hello")) must equal ("""{"helloWorld":"hello"}""")
  }

  test("Keys are underscoreized under the Underscore naming strategy") {
    @JsonKeyStrategy(Strategy.Underscore)
    case class UnderscoreKeys(helloWorld: String)
    implicit val codec = AutomaticJsonCodecBuilder[UnderscoreKeys]
    JsonUtil.renderJson(UnderscoreKeys("hello")) must equal ("""{"hello_world":"hello"}""")
  }

  test("Multiple keys for a field render as the first key") {
    case class Foo(@JsonKeys(Array("hello","world")) x: String)
    implicit val codec = AutomaticJsonCodecBuilder[Foo]
    JsonUtil.renderJson(Foo("gnu")) must equal ("""{"hello":"gnu"}""")
  }

  test("Multiple keys for a field accept any key") {
    case class Foo(@JsonKeys(Array("hello","world")) x: String)
    implicit val codec = AutomaticJsonCodecBuilder[Foo]
    JsonUtil.parseJson[Foo]("""{"hello":"gnu"}""") must equal (Right(Foo("gnu")))
    JsonUtil.parseJson[Foo]("""{"world":"gnat"}""") must equal (Right(Foo("gnat")))
  }

  test("Multiple keys for an optional field accepts no key") {
    case class Foo(@JsonKeys(Array("hello","world")) x: Option[String])
    implicit val codec = AutomaticJsonCodecBuilder[Foo]
    JsonUtil.parseJson[Foo]("""{}""") must equal (Right(Foo(None)))
  }

  test("Multiple keys for a non-optional field reports the first as misisng") {
    case class Foo(@JsonKeys(Array("hello","world")) x: String)
    implicit val codec = AutomaticJsonCodecBuilder[Foo]
    JsonUtil.parseJson[Foo]("""{}""") must equal (Left(DecodeError.MissingField("hello", Path.empty)))
  }

  test("Multiple options for the same field pick the earliest one") {
    case class Foo(@JsonKeys(Array("hello","there","world")) x: String)
    implicit val codec = AutomaticJsonCodecBuilder[Foo]
    JsonUtil.parseJson[Foo]("""{"there":"gnu","world":"gnat"}""") must equal (Right(Foo("gnu")))
    JsonUtil.parseJson[Foo]("""{"world":"gnat","there":"gnu"}""") must equal (Right(Foo("gnu")))
  }
}
