package com.rojoma.json
package ast

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object GenJValue {
  import Arbitrary.arbitrary

  val genJBoolean = for {
    x <- arbitrary[Boolean]
  } yield JBoolean(x)

  val genJFloatingPoint = for {
    x <- arbitrary[Double] suchThat { d => !d.isNaN && !d.isInfinity }
  } yield JFloatingPoint(x)

  val genJIntegral = for {
    x <- arbitrary[Long]
  } yield JIntegral(x)

  val genJNumber: Gen[JNumber] = Gen.oneOf(genJFloatingPoint, genJIntegral)
  
  def genJString(implicit arbString: Arbitrary[String]) = for {
    x <- arbString.arbitrary
  } yield JString(x)

  val genJNull: Gen[JNull] = Gen.value(JNull) // Just for completeness' sake

  def genJAtom(implicit arbString: Arbitrary[String]): Gen[JAtom] =
    Gen.oneOf(genJNull, genJBoolean, genJNumber, genJString)

  def genJArray(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5): Gen[JArray] = Gen.sized { sz =>
    for {
      x <- Gen.containerOf[List, JValue](Gen.resize((sz * sizeFactor).toInt, elementGen))
    } yield JArray(x)
  }

  def genJObject(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5)(implicit arbString: Arbitrary[String]): Gen[JObject] = Gen.sized { sz =>
    def pairGen = for {
      x <- arbString.arbitrary
      y <- elementGen
    } yield (x, y)

    for {
      x <- Gen.containerOf[List, (String, JValue)](Gen.resize((sz * sizeFactor).toInt, pairGen))
    } yield JObject(x.toMap)
  }

  def genJCompound(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5)(implicit arbString: Arbitrary[String]): Gen[JCompound] =
    Gen.oneOf(genJArray(elementGen, sizeFactor), genJObject(elementGen, sizeFactor))

  def genJValue(implicit arbString: Arbitrary[String]): Gen[JValue] =
    Gen.oneOf(genJAtom, genJCompound(Gen.lzy(genJValue)))
}
