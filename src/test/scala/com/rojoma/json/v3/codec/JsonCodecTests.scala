package com.rojoma.json.v3
package codec

import scala.jdk.CollectionConverters._

import ast._
import interpolation._

import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.Checkers

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

class JsonCodecTests extends FunSuite with Checkers with MustMatchers {
  import JsonEncode.toJValue
  import JsonDecode.fromJValue

  def doCheck[T : Arbitrary : JsonEncode : JsonDecode](): Unit = {
    check(forAll { x: T =>
      fromJValue[T](toJValue(x)) == Right(x)
    })
  }

  test("boolean roundtrips") {
    doCheck[Boolean]()
  }

  test("string roundtrips") {
    doCheck[String]()
  }

  test("byte roundtrips") {
    doCheck[Byte]()
  }

  test("short roundtrips") {
    doCheck[Short]()
  }

  test("int roundtrips") {
    doCheck[Int]()
  }

  test("long roundtrips") {
    doCheck[Long]()
  }

  test("bigint roundtrips") {
    doCheck[BigInt]()
  }

  test("float roundtrips") {
    doCheck[Float]()
  }

  test("double roundtrips") {
    doCheck[Double]()
  }

  test("bigdecimal roundtrips") {
    doCheck[BigDecimal]()
  }

  locally {
    import testsupport.ArbitraryJValue._
    import ast._
    test("jvalue roundtrips") { doCheck[JValue]() }
    locally {
      test("jatom roundtrips") { doCheck[JAtom]() }
      locally {
        test("jnull roundtrips") { doCheck[JNull]() };
        test("jboolean roundtrips") { doCheck[JBoolean]() }
        test("jstring roundtrips") { doCheck[JString]() }
        test("jnumber roundtrips") { doCheck[JNumber]() }
      }
      test("jcompound roundtrips") { doCheck[JCompound]() }
      locally {
        test("jarray roundtrips") { doCheck[JArray]() }
        test("jobject roundtrips") { doCheck[JObject]() }
      }
    }
  }

  test("seq roundtrips") {
    doCheck[List[String]]()
  }

  test("array roundtrips") {
    check(forAll { x: Array[String] =>
      // YAY JAVA
      java.util.Arrays.equals(fromJValue[Array[String]](toJValue(x)).getOrElse(fail("ack")).asInstanceOf[Array[Object]], x.asInstanceOf[Array[Object]])
    })
  }

  test("java.util.List roundtrips") {
    import java.{util => ju}
    implicit def arbitraryJUList[T : Arbitrary] = Arbitrary {
      import Arbitrary.arbitrary
      for(xs <- arbitrary[List[T]]) yield new ju.ArrayList(xs.asJava) : ju.List[T]
    }
    doCheck[ju.List[String]]()
  }

  test("map roundtrips") {
    doCheck[Map[String, String]]()
  }

  test("java.util.Map roundtrips") {
    import java.{util => ju}
    implicit def arbitraryJUMap[T : Arbitrary, U : Arbitrary] = Arbitrary {
      import Arbitrary.arbitrary
      for(xs <- arbitrary[Map[T, U]]) yield {
        val juMap: ju.Map[T, U] = new java.util.HashMap
        juMap.putAll(xs.asJava)
        juMap
      }
    }
    doCheck[ju.Map[String, String]]()
  }

  test("scala enum roundtrips") {
    object X extends Enumeration {
      val a, b, c = Value
      val d = Value("haha")
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.encode(X.a) must equal (JString("a"))
    codec.decode(JString("b")) must equal (Right(X.b))
    codec.encode(X.d) must equal (JString("haha"))
    codec.decode(JString("haha")) must equal (Right(X.d))
  }

  test("scala enum codec disallows invalid values") {
    object X extends Enumeration {
      val a, b, c = Value
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.decode(JString("q")) must equal (Left(DecodeError.InvalidValue(JString("q"), Path())))
  }

  test("Given a choice of decode errors, longer path wins") {
    JsonDecode[Either[String, Seq[Int]]].decode(j"[false]") must equal (Left(DecodeError.InvalidType(JNumber, JBoolean, Path(0))))
    JsonDecode[Either[Seq[Int], String]].decode(j"[false]") must equal (Left(DecodeError.InvalidType(JNumber, JBoolean, Path(0))))
    JsonDecode[Either[Seq[Int], Seq[String]]].decode(j"[false]") must equal (Left(DecodeError.Multiple(Seq(DecodeError.InvalidType(JString, JBoolean, Path(0)), DecodeError.InvalidType(JNumber, JBoolean, Path(0))))))
    JsonDecode[Either[Either[Seq[Int], Seq[String]], String]].decode(j"[false]") must equal (Left(DecodeError.Multiple(Seq(DecodeError.InvalidType(JString, JBoolean, Path(0)), DecodeError.InvalidType(JNumber, JBoolean, Path(0))))))
    JsonDecode[Either[String, Either[Seq[Int], Seq[String]]]].decode(j"[false]") must equal (Left(DecodeError.Multiple(Seq(DecodeError.InvalidType(JString, JBoolean, Path(0)), DecodeError.InvalidType(JNumber, JBoolean, Path(0))))))
  }
}
