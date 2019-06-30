package com.rojoma.json.v3
package testsupport

import org.scalacheck.Arbitrary

object ArbitraryJValue {
  import GenJValue._

  implicit val ArbitraryJBoolean = Arbitrary(genJBoolean)
  implicit val ArbitraryJNumber = Arbitrary(genJNumber)
  implicit val ArbitraryJNull = Arbitrary(genJNull)
  implicit val ArbitraryJString = Arbitrary(genJString)
  implicit val ArbitraryJAtom = Arbitrary(genJAtom)
  implicit val ArbitraryJArray = Arbitrary(genJArray())
  implicit val ArbitraryJObject = Arbitrary(genJObject())
  implicit val ArbitraryJCompound = Arbitrary(genJCompound())
  implicit val ArbitraryJValue = Arbitrary(genJValue)
}
