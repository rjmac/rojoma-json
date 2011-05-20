package com.rojoma.json
package ast

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object ArbitraryJson {
  import Arbitrary.arbitrary

  val boolGen = for {
    x <- arbitrary[Boolean]
  } yield JBoolean(x)
  
  val floatGen = for {
    x <- arbitrary[Double] suchThat { d => !d.isNaN && !d.isInfinity }
  } yield JNumber(x)
  
  val intGen = for {
    x <- arbitrary[Long]
  } yield JNumber(x)
  
  val numGen = Gen.oneOf(intGen, floatGen)
  
  implicit val ArbitraryValidString = Arbitrary[String] {
    val lowSurrogate = Gen.choose(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE).map(_.toChar)

    val notLowSurrogate = Gen.frequency(
      (Character.MIN_LOW_SURROGATE - Char.MinValue, Gen.choose(Char.MinValue, Character.MIN_LOW_SURROGATE - 1)),
      (Char.MaxValue - Character.MAX_LOW_SURROGATE, Gen.choose(Character.MAX_LOW_SURROGATE + 1, Char.MaxValue))
    ).map(_.toChar)

    val validCodePoint = notLowSurrogate flatMap { a =>
      if(a.isHighSurrogate) lowSurrogate map { b => new String(Array(a, b)) }
      else a.toString
    }

    Gen.containerOf[List, String](validCodePoint) map (_.mkString)
  }

  val stringGen = for {
    x <- arbitrary[String]
  } yield JString(x)

  val arrayGen: Gen[JArray] = Gen.sized { sz =>
    for {
      x <-Gen.containerOf[List, JValue](Gen.resize(sz/2, valueGen))
    } yield JArray(x)
  }

  val objectGen: Gen[JObject] = Gen.sized { sz =>
    def pairGen = for {
      x <- arbitrary[String]
      y <- valueGen
    } yield (x, y)

    for {
      x <- Gen.containerOf[List, (String, JValue)](Gen.resize(sz/2, pairGen))
    } yield JObject(x.toMap)
  }

  val valueGen = Gen.oneOf(JNull, boolGen, numGen, stringGen, arrayGen, objectGen)

  implicit val ArbitraryJson = Arbitrary[JValue](valueGen)
}
