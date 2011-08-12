package com.rojoma.json
package diff

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.matchers.MustMatchers

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import ast.ArbitraryJValue._

import ast._

class JsonIoTests extends FunSuite with Checkers with MustMatchers {
  test("identical values have no difference") {
    check(forAll { (x: JValue) =>
      JsonDiff(x, x) == None
    })
  }

  test("different (json) types are always a Replacement") {
    check(forAll { (x: JValue, y: JValue) =>
      (x.getClass != y.getClass) ==> (JsonDiff(x,y) == Some(Replacement(x,y)))
    })
  }

  test("different atoms are always a Replacement") {
    check(forAll { (x: JAtom, y: JAtom) =>
      (x != y) ==> (JsonDiff(x,y) == Some(Replacement(x,y)))
    })
  }

  test("lengthening an array produces Additions") {
    check(forAll { (x: JArray, y: JArray) =>
      y.nonEmpty ==> (JsonDiff(x, JArray(x.toSeq ++ y.toSeq)) == Some(ArrayDiff(Stream.from(x.length).zip(y map Addition).toMap)))
    })
  }

  test("shortening an array produces Removals") {
    check(forAll { (x: JArray, y: JArray) =>
      y.nonEmpty ==> (JsonDiff(JArray(x.toSeq ++ y.toSeq), x) == Some(ArrayDiff(Stream.from(x.length).zip(y map Removal).toMap)))
    })
  }
}
