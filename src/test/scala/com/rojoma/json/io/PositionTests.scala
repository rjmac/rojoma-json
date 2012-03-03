package com.rojoma.json
package io

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._

class PositionTests extends FunSuite with Checkers {
  test("Position stores rows and columns") {
    check(forAll { (row: Int, column: Int) =>
      val pos = Position(row, column)
      row == pos.row && column == pos.column
    })
  }

  test("Position can be unapplied") {
    check(forAll { (row: Int, column: Int) =>
      Position(row, column) match {
        case Position(r,c) => row == r && column == c
        case _ => false
      }
    })
  }
}
