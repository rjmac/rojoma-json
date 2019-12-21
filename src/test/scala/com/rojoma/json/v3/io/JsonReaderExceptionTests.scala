package com.rojoma.json.v3
package io

import org.scalatest.{FunSuite, MustMatchers, EitherValues}

import codec._

class JsonReaderExceptionTests extends FunSuite with MustMatchers with EitherValues {
  test("Can roundtrip a parser EOF through JSON") {
    val ex = try { JsonReader.fromString("{"); ??? } catch { case e: JsonReaderException => e }
    JsonDecode.fromJValue[JsonReaderException](JsonEncode.toJValue(ex)).right.value mustBe a [JsonParserEOF]
  }

  test("Can roundtrip a lexer EOF through JSON") {
    val ex = try { JsonReader.fromString("'"); ??? } catch { case e: JsonReaderException => e }
    JsonDecode.fromJValue[JsonReaderException](JsonEncode.toJValue(ex)).right.value mustBe a [JsonLexerEOF]
  }
}
