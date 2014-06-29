package com.rojoma.json.v3
package io

class Position(val `private once 2.10 is no more`: Long) extends AnyVal {
  def row = (`private once 2.10 is no more` >> 32).toInt
  def column = `private once 2.10 is no more`.toInt

  def copy(row: Int = row, column: Int = column) = Position(row, column)

  def isValid = row != -1 || column != -1

  override def toString = row + ":" + column
}

object Position {
  def apply(row: Int, column: Int) = new Position(row.toLong << 32 | (column.toLong & 0xffffffffL))
  def unapply(p: Position) = Some((p.row, p.column))

  val Invalid = Position(-1, -1)
}
