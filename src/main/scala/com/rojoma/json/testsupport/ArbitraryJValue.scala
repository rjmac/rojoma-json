package com.rojoma.json
package testsupport

import ast._

import org.scalacheck.Arbitrary

object ArbitraryJValue {
  import GenJValue._

  implicit val ArbitraryJBoolean = Arbitrary(genJBoolean)
  implicit val ArbitraryJNumber = Arbitrary(genJNumber)
  implicit val ArbitraryJNull = Arbitrary(genJNull)
  implicit def ArbitraryJString(implicit arbString: Arbitrary[String]) = Arbitrary(genJString)

  implicit def ArbitraryJAtom(implicit arbString: Arbitrary[String]) = Arbitrary(genJAtom)

  implicit def ArbitraryJArray(implicit arbString: Arbitrary[String]) = Arbitrary(genJArray())
  implicit def ArbitraryJObject(implicit arbString: Arbitrary[String]) = Arbitrary(genJObject())

  implicit def ArbitraryJCompound(implicit arbString: Arbitrary[String]) = Arbitrary(genJCompound())

  implicit def ArbitraryJValue(implicit arbString: Arbitrary[String]) = Arbitrary(genJValue)
}
