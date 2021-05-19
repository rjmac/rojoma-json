package com.rojoma.json.v3
package io

import scala.reflect.ClassTag

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import ast.JValue
import testsupport.ArbitraryJValue.ArbitraryJValue

class JsonReaderIteratorComparisonTests extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  def eventReader(s: String) = new EventJsonReader(new FusedBlockJsonEventIterator(s))
  def blockReader(s: String) = new FusedBlockJsonReader(s)

  def attempt[A](a: => A): Either[Exception, A] =
    try {
      Right(a)
    } catch {
      case e: Exception => Left(e)
    }

  def compareJsonExceptions(a: Exception, b: Exception): Unit = {
    def mismatch(): Nothing =
      fail("Mismatched exceptions: " + a.getMessage + ", " + b.getMessage)

    def check[T <: Exception : ClassTag](e: Exception)(f: T => Any): Unit = {
      if(implicitly[ClassTag[T]].runtimeClass.isInstance(e)) f(e.asInstanceOf[T])
      else mismatch()
    }

    (a,b) match {
      case (nst1: NoSuchTokenException, nst2: NoSuchTokenException) =>
        nst1.position must equal (nst2.position)
      case (j1: JsonReaderException, j2: JsonReaderException) =>
        j1 match {
          case uc1: JsonUnexpectedCharacter =>
            check[JsonUnexpectedCharacter](j2) { uc2 =>
              uc1.character must equal (uc2.character)
              uc1.expected must equal (uc2.expected)
            }
          case num1: JsonNumberOutOfRange =>
            check[JsonNumberOutOfRange](j2) { num2 =>
              num1.number must equal (num2.number)
            }
          case eof1: JsonLexerEOF =>
            check[JsonLexerEOF](j2) { eof2 =>
            }
          case eof1: JsonParserEOF =>
            check[JsonParserEOF](j2) { eof2 =>
            }
          case tok1: JsonUnexpectedToken =>
            check[JsonUnexpectedToken](j2) { tok2 =>
              tok1.token must equal (tok2.token)
              tok1.token.position must equal (tok2.token.position)
              tok1.expected must equal (tok2.expected)
            }
          case unId1: JsonUnknownIdentifier =>
            check[JsonUnknownIdentifier](j2) { unId2 =>
              unId1.identifier must equal(unId2.identifier)
              unId1.position must equal (unId2.position)
            }
          case _: JsonBadParse =>
            fail("JsonBadParse thrown from reader?")
        }

        j1.position must equal (j2.position)
      case _ =>
        fail("At least one reader threw a non-rojoma-json exception: " + a.getClass.getName + ", " + b.getClass.getName)
    }
  }

  def compare(s: String): Unit = {
    val eventResult = attempt(eventReader(s).read())
    val blockResult = attempt(blockReader(s).read())

    (eventResult, blockResult) match {
      case (Right(val1), Right(val2)) =>
        val1 must equal (val2)
      case (Left(ex1), Left(ex2)) =>
        compareJsonExceptions(ex1, ex2)
      case (Right(_), Left(_)) =>
        fail("Event reader succeeded; block reader threw an exception")
      case (Left(_), Right(_)) =>
        fail("Block reader succeeded; event reader threw an exception")
    }
  }

  test("Event and block readers produce the same results on well-formed data") {
    forAll { (datum: JValue, compact: Boolean) =>
      val s = if(compact) CompactJsonWriter.toString(datum) else PrettyJsonWriter.toString(datum)
      compare(s)
    }
  }

  def withTruncatedJson(f: String => Unit): Unit = {
    forAll { (datum: JValue, compact: Boolean) =>
      val fullS = if(compact) CompactJsonWriter.toString(datum) else PrettyJsonWriter.toString(datum)
      val halfS = fullS.substring(0, fullS.length / 2)
      f(halfS)
    }
  }

  test("Event and block readers produce the same results on truncated data") {
    withTruncatedJson { s =>
      compare(s)
    }
  }

  def withBrokenJson(f: String => Unit): Unit = {
    val punct = Array(":",",","{","}", "[","]","//","/*") // It is important that there are 8 of theses
    forAll { (datum: JValue, n: Int, i: Int) =>
      val s = PrettyJsonWriter.toString(datum)
      val split = s.split(" ", -1)
      val nthSpace = if(n == Int.MinValue) 0 else (n.abs % split.length)
      val withTokenAdded = (split.take(nthSpace) ++ Array(punct(i & 7)) ++ split.drop(nthSpace)).mkString(" ")
      f(withTokenAdded)
    }
  }

  test("Event and block readers produce the same results on malformed data") {
    withBrokenJson { s =>
      compare(s)
    }
  }
}
