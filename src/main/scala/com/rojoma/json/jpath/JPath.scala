package com.rojoma.json
package jpath

import ast._
import zipper._

import JPath._

class JPath private (cursors: Stream[JsonZipper]) {
  def this(input: JValue) = this(Stream(JsonZipper(input)))

  def finish = cursors.map(_.value)

  private def step(op: Stage): JPath = {
    if(cursors.isEmpty) this
    else new JPath(cursors.flatMap(op))
  }

  def down(target: String) = step(downOp(target))
  def down(target: Int) = step(downOp(target))
  def * = step(downAllOp)
  def downLast = step(downLastOp)
  def downFirst = step(downFirstOp)
  def rec = step(recOp)
  def ** = *.rec
  def where(pred: JsonZipper => Boolean) = step(whereOp(pred))
  def downWhere(pred: JsonZipper => Boolean) = *.where(pred)

  // This is basically mark-and-return.  E.g.,
  //    x.having(_.down("foo").*.where(isNumberGreaterThan(5)))
  // is the same as:
  //    x.down("foo").*.where(isNumberGreaterThan(5)).up.up
  // but also works if one of the inner steps is "rec", where the
  // needed number of "up" calls is unknown (and indeed variable
  // depending on where in the tree a match was found).
  def having(pred: JPath => JPath) = where(z => pred(new JPath(Stream(z))).finish.nonEmpty)

  def up = step(upOp)
  def next = step(nextOp)
  def prev = step(prevOp)
}

object JPath {
  type Stage = JsonZipper => Stream[JsonZipper]

  private def downOp(target: String)(input: JsonZipper): Stream[JsonZipper] = input match {
    case obj: JObjectZipper => obj.down(target).toStream
    case _ => Stream.empty
  }

  private def downOp(target: Int)(input: JsonZipper): Stream[JsonZipper] = input match {
    case arr: JArrayZipper => arr.down(target).toStream
    case _ => Stream.empty
  }

  private val downAllOp: Stage = _ match {
    case _: JAtomZipper => Stream.empty
    case arr: JArrayZipper => Stream.range(0, arr.size).map(arr.down_!)
    case obj: JObjectZipper => (obj.value.fields.keys).toStream.map(obj.down_!)
  }

  private val downLastOp: Stage = _ match {
    case arr: JArrayZipper => arr.down(arr.size - 1).toStream
    case _ => Stream.empty
  }

  private val downFirstOp: Stage = _ match {
    case arr: JArrayZipper => arr.down(0).toStream
    case _ => Stream.empty
  }

  private val recOp: Stage = input => input #:: downAllOp(input).flatMap(recOp)

  private def whereOp(pref: JsonZipper => Boolean)(input: JsonZipper): Stream[JsonZipper] = {
    if(pref(input)) Stream(input)
    else Stream.empty
  }

  private val upOp: Stage = _.up.toStream

  private val nextOp: Stage = _.next.toStream

  private val prevOp: Stage = _.prev.toStream
}
