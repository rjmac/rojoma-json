package com.rojoma.json.v3
package testsupport

import org.scalacheck.Arbitrary

object ArbitraryJValue {
  import GenJValue._

  given ArbitraryJBoolean: Arbitrary[ast.JBoolean] = Arbitrary(genJBoolean)
  given ArbitraryJNumber: Arbitrary[ast.JNumber] = Arbitrary(genJNumber)
  given ArbitraryJNull: Arbitrary[ast.JNull] = Arbitrary(genJNull)
  given ArbitraryJString: Arbitrary[ast.JString] = Arbitrary(genJString)
  given ArbitraryJAtom: Arbitrary[ast.JAtom] = Arbitrary(genJAtom)
  given ArbitraryJArray: Arbitrary[ast.JArray] = Arbitrary(genJArray())
  given ArbitraryJObject: Arbitrary[ast.JObject] = Arbitrary(genJObject())
  given ArbitraryJCompound: Arbitrary[ast.JCompound] = Arbitrary(genJCompound())
  given ArbitraryJValue: Arbitrary[ast.JValue] = Arbitrary(genJValue)
}
