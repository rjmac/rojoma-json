package com.rojoma.json
package jpath

import ast._
import zipper._

import JPath._

class JPath private (cursors: Seq[JsonZipper[_]]) {
  def this(input: JValue) = this(Seq(JsonZipper(input)))

  def finish = cursors.map(_.here)

  private def step(op: Stage): JPath = {
    if(cursors.isEmpty) this
    else new JPath(cursors.flatMap(op).distinct)
  }

  def down(target: String) = step(downOp(target))
  def down(target: Int) = step(downOp(target))
  def * = step(downAllOp)
  def downLast = step(downLastOp)
  def downFirst = step(downFirstOp)
  def downRec(target: String) = step(downRecOp(target))
  def downRec(target: Int) = step(downRecOp(target))
  def up = step(upOp)
  def next = step(nextOp)
  def prev = step(prevOp)
}

object JPath {
  type Stage = JsonZipper[_] => Seq[JsonZipper[_]]

  private def downOp(target: String)(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input.down_?(target).toSeq
  }

  private def downOp(target: Int)(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input.down_?(target).toSeq
  }

  private def downAllOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input match {
      case _: JAtomZipper[_] => Nil
      case arr: JArrayZipper[_] => (0 until arr.size).map(arr.down)
      case obj: JObjectZipper[_] => (obj.here.fields.keys).map(obj.down).toSeq
    }
  }

  private def downLastOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input.last_?.toSeq
  }

  private def downFirstOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input.first_?.toSeq
  }

  private def downRecOp(target: String)(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input match {
      case _: JAtomZipper[_] => Nil
      case arr: JArrayZipper[_] =>
        (0 until arr.size).map(arr.down).flatMap(downRecOp(target))
      case obj: JObjectZipper[_] =>
        val subResults = (obj.here.fields.keys).map(obj.down).flatMap(downRecOp(target)).toSeq
        if(obj.here.contains(target)) obj.down(target) +: subResults
        else subResults
    }
  }

  private def downRecOp(target: Int)(input: JsonZipper[_]): Seq[JsonZipper[_]] = {
    input match {
      case _: JAtomZipper[_] => Nil
      case arr: JArrayZipper[_] =>
        val subResults = (0 until arr.size).map(arr.down).flatMap(downRecOp(target)).toSeq
        if(0 <= target && target < arr.size) arr.down(target) +: subResults
        else subResults
      case obj: JObjectZipper[_] =>
        (obj.here.fields.keys).map(obj.down).flatMap(downRecOp(target)).toSeq
    }
  }

  private def upOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = input.up_?.toSeq

  private def nextOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = input.next_?.toSeq

  private def prevOp(input: JsonZipper[_]): Seq[JsonZipper[_]] = input.prev_?.toSeq
}
