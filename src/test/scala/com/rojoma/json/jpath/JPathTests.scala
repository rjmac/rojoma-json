package com.rojoma.json
package jpath

import ast._

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class JPathTests extends FunSuite with MustMatchers {
  def j(s: String) = io.JsonReader.fromString(s)

  test("* returns all array values") {
    (new JPath(j("""[1,2,3]""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }

  test("* returns all object values") {
    (new JPath(j("""{one:1,two:2,three:3}""")).*.finish.toList) must equal (List(JNumber(1), JNumber(2), JNumber(3)))
  }
}
