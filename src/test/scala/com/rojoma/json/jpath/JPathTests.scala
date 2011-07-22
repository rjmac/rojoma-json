package com.rojoma.json
package jpath

import ast._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class JPathTests extends FunSuite with MustMatchers {
  def j(s: String) = io.JsonReader.fromString(s)

  def isNumberGreaterThan(x: Int)(f: zipper.JsonZipper[_]): Boolean = f.here match {
    case JNumber(n) => n > BigDecimal(x)
    case _ => false
  }

  test("* returns all array values") {
    (new JPath(j("""[1,2,3]""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }

  test("* returns all object values") {
    (new JPath(j("""{one:1,two:2,three:3}""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }

  test("having does a mark-and-return for non-rec calls") {
    (new JPath(j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")).*.having(_.down("foo").*.where(isNumberGreaterThan(5))).finish.toSet) must equal (Set(j("""{foo:[4,5,6]}"""),j("""{foo:[7,8,9]}""")))
  }

  test("having does a mark-and-return for rec calls") {
    (new JPath(j("""[{foo:[1,2,3]},{foo:[4,5,6]},{foo:[7,8,9]}]""")).*.having(_.rec.where(isNumberGreaterThan(5))).finish.toSet) must equal (Set(j("""{foo:[4,5,6]}"""),j("""{foo:[7,8,9]}""")))
  }
}
