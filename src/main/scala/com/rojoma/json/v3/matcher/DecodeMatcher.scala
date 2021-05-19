package com.rojoma.json.v3
package matcher

import ast._
import codec._

/** Simple class to turn a [[com.rojoma.json.v3.codec.JsonDecode]] into something
 * that can be used for pattern-matching.
 *
 * @example {{{
 * val ListOfFoo = DecodeMatcher[List[Foo]]
 * val NamedBars = DecodeMatcher[Map[String, Bar]]
 * value match {
 *   case ListOfFoo(foos) => foos.foreach(_.fooify())
 *   case NamedBars(bars) => bars("hello").doBarring()
 *   case _ => // didn't match
 * }
 * }}}
 */
class DecodeMatcher[T](using codec: JsonDecode[T]) {
  def unapply(j: JValue) = codec.decode(j).toOption
}

object DecodeMatcher {
  def apply[T: JsonDecode] = new DecodeMatcher[T]
}
