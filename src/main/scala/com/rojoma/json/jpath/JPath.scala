package com.rojoma.json
package jpath

import ast._
import zipper._

import JPath._

class JPath private (ops: Seq[Stage]) {
  def this() = this(Vector.empty)

  def apply(source: JValue) = apply2(Seq(JsonZipper(source)), ops.toStream).map(_.here).distinct

  private def apply2(source: Seq[JsonZipper[_]], ops: Stream[Stage]): Seq[JsonZipper[_]] = {
    ops match {
      case op #:: remainder =>
        val newSource = for {
          start <- source
          end <- op(start)
        } yield end
        if(newSource.isEmpty) newSource
        else apply2(newSource.distinct, remainder)
      case _ =>
        source
    }
  }

  def down(target: String) = new JPath(ops :+ downOp(target)_)
  def down(target: Int) = new JPath(ops :+ downOp(target)_)
  def * = new JPath(ops :+ downAllOp _)
  def downLast = new JPath(ops :+ downLastOp _)
  def downFirst = new JPath(ops :+ downFirstOp _)
  def downRec(target: String) = new JPath(ops :+ downRecOp(target)_)
  def downRec(target: Int) = new JPath(ops :+ downRecOp(target)_)
  def up = new JPath(ops :+ upOp _)
  def next = new JPath(ops :+ nextOp _)
  def prev = new JPath(ops :+ prevOp _)
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
