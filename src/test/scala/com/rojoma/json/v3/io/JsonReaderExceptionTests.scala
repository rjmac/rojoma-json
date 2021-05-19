package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

import codec._

class JsonReaderExceptionTests extends AnyFunSuite with Matchers {
  test("Can roundtrip a parser EOF through JSON") {
    val ex = try { JsonReader.fromString("{"); ??? } catch { case e: JsonReaderException => e }
    JsonDecode.fromJValue[JsonReaderException](JsonEncode.toJValue(ex)).getOrElse(fail("ack")) mustBe a [JsonParserEOF]
  }

  test("Can roundtrip a lexer EOF through JSON") {
    val ex = try { JsonReader.fromString("'"); ??? } catch { case e: JsonReaderException => e }
    JsonDecode.fromJValue[JsonReaderException](JsonEncode.toJValue(ex)).getOrElse(fail("ack")) mustBe a [JsonLexerEOF]
  }
}
