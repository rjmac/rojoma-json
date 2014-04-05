package com.rojoma.json.v2
package io

class Position(val __rowCol: Long) extends AnyVal {
  def row = (__rowCol >> 32).toInt
  def column = __rowCol.toInt

  def copy(row: Int = row, column: Int = column) = Position(row, column)

  def isValid = row != -1 || column != -1

  override def toString = row + ":" + column
}

object Position {
  def apply(row: Int, column: Int) = new Position(row.toLong << 32 | (column.toLong & 0xffffffffL))
  def unapply(p: Position) = Some((p.row, p.column))

  val Invalid = Position(-1, -1)
}
