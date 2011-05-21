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

  locally {
    import ast.ArbitraryJValue._
    import ast._
    test("jvalue roundtrips") { doCheck[JValue] }
    locally {
      test("jatom roundtrips") { doCheck[JAtom] }
      locally {
        test("jnull roundtrips") { doCheck[JNull] };
        test("jboolean roundtrips") { doCheck[JBoolean] }
        test("jstring roundtrips") { doCheck[JString] }
        test("jnumber roundtrips") { doCheck[JNumber] }
        locally {
          test("jintegral roundtrips") { doCheck[JIntegral] }
          test("jfloatingpoint roundtrips") { doCheck[JFloatingPoint] }
        }
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
