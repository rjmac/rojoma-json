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

class JsonDiffTests extends FunSuite with Checkers with MustMatchers {
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
      y.nonEmpty ==> (JsonDiff(x, JArray(x.toSeq ++ y.toSeq)) == Some(ArrayDiffLengthening(Map.empty, y.toSeq)))
    })
  }

  test("shortening an array produces Removals") {
    check(forAll { (x: JArray, y: JArray) =>
      y.nonEmpty ==> (JsonDiff(JArray(x.toSeq ++ y.toSeq), x) == Some(ArrayDiffShortening(Map.empty, y.toSeq)))
    })
  }

  test("adding a field produces an Addition") {
    check(forAll { (x: JObject, k: String, y: JValue) =>
      (!x.contains(k)) ==> (JsonDiff(x, JObject(x.fields + (k -> y))) == Some(ObjectDiff(Map(k -> Addition(y)))))
    })
  }

  val genObjectWithField = for {
    obj <- arbitrary[JObject] suchThat (_.nonEmpty)
    i <- Gen.choose(0, obj.size - 1)
  } yield (obj, obj.keys.toSeq(i))

  test("replacing a field with an atom produces a Replacement") {
    check(forAll(genObjectWithField, arbitrary[JAtom]) { (objField, atom) =>
      val (obj, field) = objField
      (obj(field) != atom) ==> (JsonDiff(obj, JObject(obj.fields + (field -> atom))) == Some(ObjectDiff(Map(field -> Replacement(obj(field), atom)))))
    })
  }

  test("replacing a field with a value of a different type produces a Replacement") {
    check(forAll(genObjectWithField, arbitrary[JValue]) { (objField, v) =>
      val (obj, field) = objField
      (obj(field).getClass != v.getClass) ==> (JsonDiff(obj, JObject(obj.fields + (field -> v))) == Some(ObjectDiff(Map(field -> Replacement(obj(field), v)))))
    })
  }

  test("removing a field produces a Removal") {
    check(forAll(genObjectWithField) { case (obj, field) =>
      JsonDiff(obj, JObject(obj.fields - field)) == Some(ObjectDiff(Map(field -> Removal(obj(field)))))
    })
  }

  // these aren't the greatest tests in the world
  // because it's so unlikely that two randomly-generated jvalues will produce an interesting diff
  // but they are nonetheless true properties
  test("applying the diff between two objects produces the second object") {
    check(forAll { (a: JValue, b: JValue) =>
      val maybeDiff = JsonDiff(a, b)
      (maybeDiff != None) ==> (maybeDiff.get.applyTo(a) == b)
    })
  }
  test("applying the reversed diff between two objects produces the first object") {
    check(forAll { (a: JValue, b: JValue) =>
      val maybeDiff = JsonDiff(a, b)
      (maybeDiff != None) ==> (maybeDiff.get.reverse.applyTo(b) == a)
    })
  }
}
