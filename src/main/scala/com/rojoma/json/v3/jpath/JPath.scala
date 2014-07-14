package com.rojoma.json.v3
package jpath

import ast._
import zipper._

import JPathImpl._

/** A high-level interface for walking down a tree defined by a
 * [[com.rojoma.json.v3.ast.JValue]], returning a set of sub-values
 * that match a path defined by a series of steps.
 *
 * @example {{{
 * val v = JsonReader.fromString("""
 *   {
 *      "array": [1,2,3],
 *      "object": {
 *        "France" : [ "Paris", "Lyon", "Bordeaux" ],
 *        "Germany" : [ "Berlin", "Munich", "Leipzig" ],
 *        "Italy" : [ "Rome", "Florence", "Milan" ]
 *      }
 *   }
 * """)
 *
 * def contains(z: JsonZipper, s: String) =
 *   z.asArray.map(_.value.toSeq.contains(JString(s))).getOrElse(false)
 *
 * JPath(v).down("array").finish
 *    // --> Stream(JArray(JNumber(1), JNumber(2), JNumber(3)))
 * JPath(v).down("object").*.downFirst.finish
 *    // --> Stream(JString("Paris"), JString("Berlin"), JString("Rome"))
 * JPath(v).*.down("France").downFirst.finish
 *    // --> Stream(JString("Paris"))
 * JPath(v).**.downFirst.finish
 *   // --> Stream(JNumber(1), JString("Paris"), JString("Berlin"), JString("Rome"))
 * JPath(v).*.having(_.*.where(contains(_, "Lyon"))).finish
 *   // --> Stream(JObject("France" -> ..., "Germany" -> ..., "Italy" -> ...))
 * JPath(v).*.*.where(contains(_, "Lyon")).finish
 *   // --> Stream(JArray(JString("Paris"), JString("Lyon"), JString("Bordeaux")))
 * }}} */
class JPath private (cursors: Stream[JsonZipper]) {
  def this(input: JValue) = this(Stream(JsonZipper(input)))

  /** Produce a `Stream` of [[com.rojoma.json.v3.ast.JValue]]s, one
   * for each current point. */
  def finish: Stream[JValue] = cursors.map(_.value)

  private def step(op: Stage): JPath = {
    if(cursors.isEmpty) this
    else new JPath(cursors.flatMap(op))
  }

  /** Go down into the named field.  If a current point is
   * not a [[com.rojoma.json.v3.ast.JObject]], or does not contain the field,
   * it is dropped.*/
  def down(target: String) = step(downOp(target))

  /** Go down into the given index.  If a current point is
   * not a [[com.rojoma.json.v3.ast.JArray]], or does not contain the index,
   * it is dropped.*/
  def down(target: Int) = step(downOp(target))

  /** Go down into all children, whether fields or array elements.  If
   * a current point is not a [[com.rojoma.json.v3.ast.JObject]] or
   * [[com.rojoma.json.v3.ast.JArray]], it is dropped. */
  def * = step(downAllOp)

  /** Go down into the last child of an array.  If a current point is not
   * a [[com.rojoma.json.v3.ast.JArray]], it is dropped. */
  def downLast = step(downLastOp)

  /** Go down into the first child of an array.  If a current point is not
   * a [[com.rojoma.json.v3.ast.JArray]], it is dropped. */
  def downFirst = step(downFirstOp)

  /** Add every descendant of the current points to the set of points. */
  def rec = step(recOp)

  /** Go down (as by `*`) and then add every descendant of the current
   * points to the set of points (as by `rec`). */
  def ** = *.rec

  /** Filter the set of current points by a predicate. */
  def where(pred: JsonZipper => Boolean) = step(whereOp(pred))

  /** Go down into all children (as by `*`) and then filter by
   * a predicate (as by `where`). */
  def downWhere(pred: JsonZipper => Boolean) = *.where(pred)

  /** Filter the current set of points by applying another
   * JPath operation to them and keeping only the ones that
   * ended up with a non-empty set of results.
   *
   * This is basically mark-and-return.  E.g.,
   * {{{
   * x.having(_.down("foo").*.where(isNumberGreaterThan(5)))
   * }}}
   * is the same as:
   * {{{
   * x.down("foo").*.where(isNumberGreaterThan(5)).up.up
   * }}}
   * but also works if one of the inner steps is "rec", where
   * no fixed number of final `up` steps would suffice.
   */
  def having(pred: JPath => JPath) = where(z => pred(new JPath(Stream(z))).finish.nonEmpty)

  /** Go up to the parents of the current points.  Any that were
   * already at the top of the tree will be dropped. */
  def up = step(upOp)

  /** Move to the next sibling.  Any points that were at the end
   * already, or which were not children of [[com.rojoma.json.v3.ast.JArray]]s,
   * will be dropped. */
  def next = step(nextOp)

  /** Move to the previous sibling.  Any points that were at the start
   * already, or which were not children of [[com.rojoma.json.v3.ast.JArray]]s,
   * will be dropped. */
  def prev = step(prevOp)
}

object JPath extends (JValue => JPath) {
  def apply(v: JValue) = new JPath(v)
}

private [jpath] object JPathImpl {
  type Stage = JsonZipper => Stream[JsonZipper]

   def downOp(target: String)(input: JsonZipper): Stream[JsonZipper] = input match {
    case obj: JObjectZipper => obj.down(target).toStream
    case _ => Stream.empty
  }

   def downOp(target: Int)(input: JsonZipper): Stream[JsonZipper] = input match {
    case arr: JArrayZipper => arr.down(target).toStream
    case _ => Stream.empty
  }

   val downAllOp: Stage = _ match {
    case _: JAtomZipper => Stream.empty
    case arr: JArrayZipper => Stream.range(0, arr.size).map(arr.down_!)
    case obj: JObjectZipper => (obj.value.fields.keys).toStream.map(obj.down_!)
  }

   val downLastOp: Stage = _ match {
    case arr: JArrayZipper => arr.down(arr.size - 1).toStream
    case _ => Stream.empty
  }

   val downFirstOp: Stage = _ match {
    case arr: JArrayZipper => arr.down(0).toStream
    case _ => Stream.empty
  }

   val recOp: Stage = input => input #:: downAllOp(input).flatMap(recOp)

   def whereOp(pref: JsonZipper => Boolean)(input: JsonZipper): Stream[JsonZipper] = {
    if(pref(input)) Stream(input)
    else Stream.empty
  }

   val upOp: Stage = _.up.toStream

   val nextOp: Stage = _.next.toStream

   val prevOp: Stage = _.prev.toStream
}
