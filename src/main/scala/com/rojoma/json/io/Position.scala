package com.rojoma.json
package io

class Position(val __rowCol: Long) extends com.rojoma.`json-impl`.io.PositionSuperClassHolder.SuperClass {
  def row = (__rowCol >> 32).toInt
  def column = __rowCol.toInt

  def copy(row: Int = row, column: Int = column) = Position(row, column)

  override def toString = row + ":" + column
}

object Position {
  def apply(row: Int, column: Int) = new Position(row.toLong << 32 | (column.toLong & 0xffffffffL))
  def unapply(p: Position) = Some((p.row, p.column))
}
