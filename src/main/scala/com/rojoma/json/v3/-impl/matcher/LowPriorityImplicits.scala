package com.rojoma.json.v3
package `-impl`.matcher

import matcher._
import codec._
import ast.JValue

class LowPriorityImplicits {
  /** Converts an object with a [[com.rojoma.json.v3.codec.JsonDecode]] into a
   * [[com.rojoma.json.v3.matcher.Pattern]] which matches a value only
   * if the codec can decode it into something which is `equal` to the
   * object. */
  given litifyDecode[T : JsonDecode]: Conversion[T, Pattern] with {
    def apply(lit: T) =
      new Pattern {
        def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
          JsonDecode[T].decode(x) match {
            case Right(y) if lit == y => Right(environment)
            case Right(_) => Left(DecodeError.InvalidValue(x))
            case Left(err) => Left(err)
          }

        def generate(environment: Pattern.Results): Option[JValue] = None
      }
  }
}
