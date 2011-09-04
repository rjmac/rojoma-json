package com.rojoma.json
package matcher

import ast._
import codec._

/** Pattern-matching shim for a Codec object.
 * {{{
 * val ListOfFoo = CodecMatcher[List[Foo]]
 * val NamedBars = CodecMatcher[Map[String, Bar]]
 * value match {
 *   case ListOfFoo(foos) => ...
 *   case NamedBars(bars) => ...
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
