package com.rojoma.json
package matcher

import ast._
import codec._

/** Simple class to turn a [[com.rojoma.json.codec.JsonCodec]] into something
 * that can be used for pattern-matching.
 *
 * @example {{{
 * val ListOfFoo = CodecMatcher[List[Foo]]
 * val NamedBars = CodecMatcher[Map[String, Bar]]
 * value match {
 *   case ListOfFoo(foos) => foos.foreach(_.fooify())
 *   case NamedBars(bars) => bars("hello").doBarring()
 *   case _ => // didn't match
 * }
 * }}}
 */
class CodecMatcher[T](implicit codec: JsonCodec[T]) {
  def unapply(j: JValue) = codec.decode(j)
}

object CodecMatcher {
  def apply[T: JsonCodec] = new CodecMatcher[T]
}
