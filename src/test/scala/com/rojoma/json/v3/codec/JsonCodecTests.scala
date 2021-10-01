package com.rojoma.json.v3
package codec

import scala.jdk.CollectionConverters._

import ast._
import interpolation._
import util.{JsonCaseInsensitiveEnum, JsonEnumStrategy, Strategy}

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

  test("scala enum roundtrips - case sensitive, identity") {
    object X extends Enumeration {
      val anEnum, b, c = Value
      val d = Value("haha")
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.encode(X.anEnum) must equal (JString("anEnum"))
    codec.decode(JString("anEnum")) must equal (Right(X.anEnum))
    codec.decode(JString("anenum")) must equal (Left(DecodeError.InvalidValue(got = JString("anenum"))))
    codec.decode(JString("b")) must equal (Right(X.b))
    codec.encode(X.d) must equal (JString("haha"))
    codec.decode(JString("haha")) must equal (Right(X.d))
    codec.decode(JString("B")) must equal (Left(DecodeError.InvalidValue(got = JString("B"))))
  }

  test("scala enum roundtrips - case insensitive - identity") {
    @JsonCaseInsensitiveEnum
    object X extends Enumeration {
      val anEnum, b, c = Value
      val d = Value("haha")
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.encode(X.anEnum) must equal (JString("anEnum"))
    codec.decode(JString("anEnum")) must equal (Right(X.anEnum))
    codec.decode(JString("anenum")) must equal (Right(X.anEnum))
    codec.decode(JString("b")) must equal (Right(X.b))
    codec.encode(X.d) must equal (JString("haha"))
    codec.decode(JString("haha")) must equal (Right(X.d))
    codec.decode(JString("HAHA")) must equal (Right(X.d))
    codec.decode(JString("B")) must equal (Right(X.b))
  }

  test("scala enum roundtrips - case sensitive, underscore") {
    @JsonEnumStrategy(Strategy.Underscore)
    object X extends Enumeration {
      val anEnum, b, c = Value
      val d = Value("haha")
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.encode(X.anEnum) must equal (JString("an_enum"))
    codec.decode(JString("an_enum")) must equal (Right(X.anEnum))
    codec.decode(JString("an_Enum")) must equal (Left(DecodeError.InvalidValue(got = JString("an_Enum"))))
    codec.decode(JString("b")) must equal (Right(X.b))
    codec.encode(X.d) must equal (JString("haha"))
    codec.decode(JString("haha")) must equal (Right(X.d))
    codec.decode(JString("B")) must equal (Left(DecodeError.InvalidValue(got = JString("B"))))
  }

  test("scala enum roundtrips - case insensitive - underscore") {
    @JsonCaseInsensitiveEnum
    @JsonEnumStrategy(Strategy.Underscore)
    object X extends Enumeration {
      val anEnum, b, c = Value
      val d = Value("haha")
    }
    val codec = JsonCodec.scalaEnumCodec(X)
    codec.encode(X.anEnum) must equal (JString("an_enum"))
    codec.decode(JString("an_enum")) must equal (Right(X.anEnum))
    codec.decode(JString("an_Enum")) must equal (Right(X.anEnum))
    codec.decode(JString("b")) must equal (Right(X.b))
    codec.encode(X.d) must equal (JString("haha"))
    codec.decode(JString("haha")) must equal (Right(X.d))
    codec.decode(JString("B")) must equal (Right(X.b))
  }

  test("java enum roundtrips - case sensitive, identity") {
    import jsoncodectests.{CaseSensitiveIdentity => X}
    val enc = implicitly[JsonEncode[X]]; val dec= implicitly[JsonDecode[X]]
    enc.encode(X.HelloWorld) must equal (JString("HelloWorld"))
    dec.decode(JString("HelloWorld")) must equal (Right(X.HelloWorld))
    dec.decode(JString("helloworld")) must equal (Left(DecodeError.InvalidValue(got = JString("helloworld"))))
    dec.decode(JString("B")) must equal (Right(X.B))
    dec.decode(JString("b")) must equal (Left(DecodeError.InvalidValue(got = JString("b"))))
  }

  test("java enum roundtrips - case insensitive - identity") {
    import jsoncodectests.{CaseInsensitiveIdentity => X}
    val enc = implicitly[JsonEncode[X]]; val dec= implicitly[JsonDecode[X]]
    enc.encode(X.HelloWorld) must equal (JString("HelloWorld"))
    dec.decode(JString("HelloWorld")) must equal (Right(X.HelloWorld))
    dec.decode(JString("helloworld")) must equal (Right(X.HelloWorld))
    dec.decode(JString("B")) must equal (Right(X.B))
    dec.decode(JString("b")) must equal (Right(X.B))
  }

  test("java enum roundtrips - case sensitive, underscore") {
    import jsoncodectests.{CaseSensitiveUnderscore => X}
    val enc = implicitly[JsonEncode[X]]; val dec= implicitly[JsonDecode[X]]
    enc.encode(X.HelloWorld) must equal (JString("hello_world"))
    dec.decode(JString("Hello_World")) must equal (Left(DecodeError.InvalidValue(got = JString("Hello_World"))))
    dec.decode(JString("hello_world")) must equal (Right(X.HelloWorld))
    dec.decode(JString("B")) must equal (Left(DecodeError.InvalidValue(got = JString("B"))))
    dec.decode(JString("b")) must equal (Right(X.B))
  }

  test("java enum roundtrips - case insensitive - underscore") {
    import jsoncodectests.{CaseInsensitiveUnderscore => X}
    val enc = implicitly[JsonEncode[X]]; val dec= implicitly[JsonDecode[X]]
    enc.encode(X.HelloWorld) must equal (JString("hello_world"))
    dec.decode(JString("Hello_World")) must equal (Right(X.HelloWorld))
    dec.decode(JString("hello_world")) must equal (Right(X.HelloWorld))
    dec.decode(JString("B")) must equal (Right(X.B))
    dec.decode(JString("b")) must equal (Right(X.B))
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
