package com.rojoma.json
package ast

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object GenJValue {
  import Arbitrary.arbitrary

  val genJBoolean = for {
    x <- arbitrary[Boolean]
  } yield JBoolean(x)

  val genJNumber: Gen[JNumber] = for {
    unscaledVal <- arbitrary[math.BigInt]
    scale <- Gen.choose(-Int.MaxValue, Int.MaxValue) // MinValue itself isn't a valid scale
  } yield JNumber(math.BigDecimal(unscaledVal, scale))
  
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
