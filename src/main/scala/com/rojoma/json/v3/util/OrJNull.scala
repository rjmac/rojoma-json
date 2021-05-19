package com.rojoma.json.v3.util

import com.rojoma.json.v3.ast.JNull
import com.rojoma.json.v3.codec.JsonEncode

/** A helper class primary for use with the interpolator.  Import
  * `OrJNull.implicits._` to get an `orJNull` method on `Option[T]`
  * which encodes the value using `T`'s `JsonEncode` when it's `Some`
  * and as `JNull` when it's `None`.
  *
  * {{{
  * val x = Some(AnEncodableThing(...))
  * val y = Option.empty[AnEncodableThing]
  * json""" {
  *   x1 : \${x.orJNull},  // These two lines
  *   x2 : ?\$x,           // encode the same way.
  *   y1 : \${y.orJNull},  // This one produces `null`.
  *   y2 : ?\$y            // And this one is elided.
  * } """
  * }}}
  */
enum OrJNull[+T] {
  case Some(value: T)
  case None
}
object OrJNull {

  given [T: JsonEncode]: JsonEncode[OrJNull[T]] with {
    def encode(x: OrJNull[T]) =
      x match {
        case Some(v) => JsonEncode.toJValue(v)
        case None => JNull
      }
  }

  object implicits {
    extension [T](underlying: Option[T]) {
      def orJNull: OrJNull[T] =
        underlying match {
          case scala.Some(x) => Some(x)
          case scala.None => None
        }
    }
  }
}
