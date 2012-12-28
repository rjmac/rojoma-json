package com.rojoma.json.util

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

import com.rojoma.json.ast._
import com.rojoma.json.codec.JsonCodec

case class Options(a: Option[Int], @NullForNone b: Option[Int], c: Int)
object Options {
  implicit val codec = AutomaticJsonCodecBuilder[Options]
}

case class Recursive(x: String, @LazyCodec xs: List[Recursive])
object Recursive {
  implicit val codec: JsonCodec[Recursive] = AutomaticJsonCodecBuilder[Recursive]
}

case class DefaultKeys(helloWorld: String)
object DefaultKeys {
  implicit val codec = AutomaticJsonCodecBuilder[DefaultKeys]
}

@JsonKeyStrategy(Strategy.Identity)
case class IdentityKeys(helloWorld: String)
object IdentityKeys {
  implicit val codec = AutomaticJsonCodecBuilder[IdentityKeys]
}

@JsonKeyStrategy(Strategy.Underscore)
case class UnderscoreKeys(helloWorld: String)
object UnderscoreKeys {
  implicit val codec = AutomaticJsonCodecBuilder[UnderscoreKeys]
}

class AutomaticJsonCodecBuilderTest extends FunSuite with MustMatchers {
  test("Options encode properly") {
    JsonUtil.renderJson(Options(None, None, 5)) must equal ("""{"b":null,"c":5}""")
    JsonUtil.renderJson(Options(Some(1), Some(2), 3)) must equal ("""{"a":1,"b":2,"c":3}""")
  }

  test("Recursive encodes properly") {
    JsonUtil.renderJson(Recursive("b",List(Recursive("a",Nil), Recursive("c",Nil)))) must equal ("""{"x":"b","xs":[{"x":"a","xs":[]},{"x":"c","xs":[]}]}""")
  }

  test("By default keys are preserved") {
    JsonUtil.renderJson(DefaultKeys("hello")) must equal ("""{"helloWorld":"hello"}""")
  }

  test("Keys are preserved under the Identity naming strategy") {
    JsonUtil.renderJson(IdentityKeys("hello")) must equal ("""{"helloWorld":"hello"}""")
  }

  test("Keys are underscoreized under the Underscore naming strategy") {
    JsonUtil.renderJson(UnderscoreKeys("hello")) must equal ("""{"hello_world":"hello"}""")
  }
}

