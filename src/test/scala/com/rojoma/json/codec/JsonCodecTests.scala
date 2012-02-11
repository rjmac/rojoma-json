package com.rojoma.json
package codec

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

class JsonCodecTests extends FunSuite with Checkers {
  import JsonCodec.{toJValue, fromJValue}

  def doCheck[T : ClassManifest : Arbitrary : JsonCodec]() {
    check(forAll { x: T =>
      fromJValue[T](toJValue(x)) == Some(x)
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

  // Unfortunately, scalacheck's Arbitrary[BigDecimal] instance
  // occasionally explodes...  I've sent a patch!
  // test("bigdecimal roundtrips") {
  //   doCheck[BigDecimal]()
  // }

  locally {
    import testsupport.ArbitraryJValue._
    import ast._
    test("jvalue roundtrips") { doCheck[JValue] }
    locally {
      test("jatom roundtrips") { doCheck[JAtom] }
      locally {
        test("jnull roundtrips") { doCheck[JNull] };
        test("jboolean roundtrips") { doCheck[JBoolean] }
        test("jstring roundtrips") { doCheck[JString] }
        test("jnumber roundtrips") { doCheck[JNumber] }
      }
      test("jcompound roundtrips") { doCheck[JCompound] }
      locally {
        test("jarray roundtrips") { doCheck[JArray] }
        test("jobject roundtrips") { doCheck[JObject] }
      }
    }
  }

  test("seq roundtrips") {
    doCheck[List[String]]()
  }

  test("array roundtrips") {
    check(forAll { x: Array[String] =>
      // YAY JAVA
      java.util.Arrays.equals(fromJValue[Array[String]](toJValue(x)).get.asInstanceOf[Array[Object]], x.asInstanceOf[Array[Object]])
    })
  }

  test("java.util.List roundtrips") {
    import java.{util => ju}
    implicit def arbitraryJUList[T : Arbitrary] = Arbitrary {
      import scala.collection.JavaConversions._
      import Arbitrary.arbitrary
      for(xs <- arbitrary[List[T]]) yield new ju.ArrayList(xs) : ju.List[T]
    }
    doCheck[ju.List[String]]()
  }

  test("map roundtrips") {
    doCheck[Map[String, String]]()
  }

  test("java.util.Map roundtrips") {
    import java.{util => ju}
    implicit def arbitraryJUMap[T : Arbitrary, U : Arbitrary] = Arbitrary {
      import scala.collection.JavaConversions._
      import Arbitrary.arbitrary
      for(xs <- arbitrary[Map[T, U]]) yield {
        val juMap: ju.Map[T, U] = new java.util.HashMap
        juMap.putAll(xs)
        juMap
      }
    }
    doCheck[ju.Map[String, String]]()
  }
}
