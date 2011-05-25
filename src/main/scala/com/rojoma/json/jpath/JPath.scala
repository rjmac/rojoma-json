package com.rojoma.json
package jpath

import ast._
import zipper._

import JPath._

class JPath private (cursors: Stream[JsonZipper[_]]) {
  def this(input: JValue) = this(Stream(JsonZipper(input)))

  def finish = cursors.map(_.here)

  private def step(op: Stage): JPath = {
    if(cursors.isEmpty) this
    else new JPath(cursors.flatMap(op))
  }

  def down(target: String) = step(downOp(target))
  def down(target: Int) = step(downOp(target))
  def * = step(downAllOp)
  def downLast = step(downLastOp)
  def downFirst = step(downFirstOp)
  def downRec = step(downRecOp)
  def where(pred: JsonZipper[_] => Boolean) = step(whereOp(pred))
  def downWhere(pred: JsonZipper[_] => Boolean) = *.where(pred)
  def up = step(upOp)
  def next = step(nextOp)
  def prev = step(prevOp)
}

object JPath {
  type Stage = JsonZipper[_] => Stream[JsonZipper[_]]

  private def downOp(target: String)(input: JsonZipper[_]): Stream[JsonZipper[_]] = {
    input.down_?(target).toStream
  }

  private def downOp(target: Int)(input: JsonZipper[_]): Stream[JsonZipper[_]] = {
    input.down_?(target).toStream
  }

  private val downAllOp: Stage = _ match {
    case _: JAtomZipper[_] => Stream.empty
    case arr: JArrayZipper[_] => Stream.range(0, arr.size - 1).map(arr.down)
    case obj: JObjectZipper[_] => (obj.here.fields.keys).toStream.map(obj.down)
  }

  private val downLastOp: Stage = _.last_?.toStream

  private val downFirstOp: Stage = _.first_?.toStream

  private val downRecOp: Stage = input => input #:: downAllOp(input).flatMap(downRecOp)

  private def whereOp(pref: JsonZipper[_] => Boolean)(input: JsonZipper[_]): Stream[JsonZipper[_]] = {
    if(pref(input)) Stream(input)
    else Stream.empty
  }

  private val upOp: Stage = _.up_?.toStream

  private val nextOp: Stage = _.next_?.toStream

  private val prevOp: Stage = _.prev_?.toStream
}
