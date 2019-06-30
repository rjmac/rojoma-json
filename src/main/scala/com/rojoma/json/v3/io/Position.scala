package com.rojoma.json.v3
package io

import ast.{JValue,JNull}
import codec._

class Position(val `private once 2.10 is no more`: Long) extends AnyVal {
  def row = (`private once 2.10 is no more` >> 32).toInt
  def column = `private once 2.10 is no more`.toInt

  def copy(row: Int = row, column: Int = column) = Position(row, column)

  def isValid = row != -1 || column != -1

  override def toString = s"$row:$column"
}

object Position {
  def apply(row: Int, column: Int) = new Position(row.toLong << 32 | (column.toLong & 0xffffffffL))
  def unapply(p: Position) = Some((p.row, p.column))

  val Invalid = Position(-1, -1)

  implicit val jCodec: JsonEncode[Position] with JsonDecode[Position] = new JsonEncode[Position] with JsonDecode[Position] {
    private val pairEnc = JsonEncode[(Int, Int)]
    private val pairDec = JsonDecode[Either[JNull, (Int, Int)]]

    def encode(p: Position) =
      if(p == Invalid) JNull
      else pairEnc.encode((p.row, p.column))
    def decode(x: JValue) =
      pairDec.decode(x).map {
        case Right((r,c)) =>
          Position(r, c)
        case Left(JNull) =>
          Invalid
      }
  }
}
