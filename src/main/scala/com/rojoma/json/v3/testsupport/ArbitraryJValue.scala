package com.rojoma.json.v3
package testsupport

import org.scalacheck.Arbitrary

object ArbitraryJValue {
  import GenJValue._

  @inline
  private def unused[T](x: T): T = {
    x
  }

  implicit val ArbitraryJBoolean = Arbitrary(genJBoolean)
  implicit val ArbitraryJNumber = Arbitrary(genJNumber)
  implicit val ArbitraryJNull = Arbitrary(genJNull)
  implicit def ArbitraryJString(implicit arbString : Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJString)
  }

  implicit def ArbitraryJAtom(implicit arbString: Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJAtom)
  }

  implicit def ArbitraryJArray(implicit arbString: Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJArray())
  }

  implicit def ArbitraryJObject(implicit arbString: Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJObject())
  }

  implicit def ArbitraryJCompound(implicit arbString: Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJCompound())
  }

  implicit def ArbitraryJValue(implicit arbString: Arbitrary[String]) = {
    unused(arbString)
    Arbitrary(genJValue)
  }
}
