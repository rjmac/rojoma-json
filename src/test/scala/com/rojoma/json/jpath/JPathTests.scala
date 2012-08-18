package com.rojoma.json
package jpath

import ast._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class JPathTests extends FunSuite with MustMatchers {
  def j(s: String) = io.JsonReader.fromString(s)

  def isNumberGreaterThan(x: Int)(f: zipper.JsonZipper): Boolean = f.value match {
    case JNumber(n) => n > BigDecimal(x)
    case _ => false
  }

  test("* returns all array values") {
    (JPath(j("""[1,2,3]""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }

  test("* returns all object values") {
    (JPath(j("""{one:1,two:2,three:3}""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }

  test("having does a mark-and-return for non-rec calls") {
    (JPath(j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")).*.having(_.down("foo").*.where(isNumberGreaterThan(5))).finish.toSet) must equal (Set(j("""{foo:[4,5,6]}"""),j("""{foo:[7,8,9]}""")))
  }

  test("having does a mark-and-return for rec calls") {
    (JPath(j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")).*.having(_.rec.where(isNumberGreaterThan(5))).finish.toSet) must equal (Set(j("""{foo:[4,5,6]}"""),j("""{foo:[7,8,9]}""")))
  }

  test("rec includes the current node") {
    val orig = j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")
    (JPath(orig).rec.where(_.value.isInstanceOf[JArray]).finish.toSet) must equal (Set(orig, j("""[1,2,3]"""), j("""[4,5,6]"""),j("""[7,8,9]""")))
  }

  test("** is the equivalent of *-and-rec") {
    val orig = j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")
    (JPath(orig).**.where(_.value.isInstanceOf[JArray]).finish.toSet) must equal (Set(j("""[1,2,3]"""), j("""[4,5,6]"""),j("""[7,8,9]""")))
  }
}
