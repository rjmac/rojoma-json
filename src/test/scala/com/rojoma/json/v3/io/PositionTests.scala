package com.rojoma.json.v3
package io

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

import org.scalacheck.Prop._

class PositionTests extends AnyFunSuite with Checkers {
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
      }
    })
  }
}
