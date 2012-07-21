package com.rojoma.json
package testsupport

import ast._

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object GenJValue {
  import Arbitrary.arbitrary

  import ArbitraryValidString._

  val genJBoolean = for {
    x <- arbitrary[Boolean]
  } yield JBoolean(x)

  private val safeFloat = for {
    i <- arbitrary[BigInt]
    s <- Gen.choose(Int.MinValue + 1, Int.MaxValue)
  } yield BigDecimal(i, s, java.math.MathContext.UNLIMITED)

  val genJNumber: Gen[JNumber] = {
    val genJInteger = arbitrary[BigInt].map(JNumber.apply)
    val genJFloatingPoint = safeFloat.map(JNumber.apply)
    Gen.oneOf(genJInteger, genJFloatingPoint)
  }

  def genJString = for {
    x <- ArbitraryValidString.arbitrary
  } yield JString(x)

  val genJNull: Gen[JNull] = Gen.value(JNull) // Just for completeness' sake

  def genJAtom: Gen[JAtom] =
    Gen.oneOf(genJNull, genJBoolean, genJNumber, genJString)

  def genJArray(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5): Gen[JArray] = Gen.sized { sz =>
    for {
      x <- Gen.containerOf[List, JValue](Gen.resize((sz * sizeFactor).toInt, elementGen))
    } yield JArray(x)
  }

  def genJObject(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5): Gen[JObject] = Gen.sized { sz =>
    def pairGen = for {
      x <- ArbitraryValidString.arbitrary
      y <- elementGen
    } yield (x, y)

    for {
      x <- Gen.containerOf[List, (String, JValue)](Gen.resize((sz * sizeFactor).toInt, pairGen))
    } yield JObject(x.toMap)
  }

  def genJCompound(elementGen: Gen[JValue] = genJValue, sizeFactor: Double = 0.5): Gen[JCompound] =
    Gen.oneOf(genJArray(elementGen, sizeFactor), genJObject(elementGen, sizeFactor))

  def genJValue: Gen[JValue] =
    Gen.oneOf(genJAtom, genJCompound(Gen.lzy(genJValue)))
}
