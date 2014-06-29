package com.rojoma.json.v3
package `-impl`.matcher

import scala.language.implicitConversions

import matcher._
import codec._
import ast.JValue

trait ReallyDecode[T] extends FLiteral {
  def decode: JsonDecode[T]
  override def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
    super.evaluate(x, environment) match {
      case r@Right(_) => r
      case l@Left(err) =>
        val choices = decode.acceptTypes
        if(choices(x.jsonType)) l
        else if(choices.size == 1)  Left(DecodeError.InvalidType(choices.iterator.next(), x.jsonType, Path.empty))
        else Left(DecodeError.Multiple(choices.toSeq.map(DecodeError.InvalidType(_, x.jsonType, Path.empty)), Path.empty))
    }
}

class LowPriorityImplicits {
  /** Converts an object with a [[com.rojoma.json.v3.codec.JsonDecode]] into a
   * [[com.rojoma.json.v3.matcher.Pattern]] which matches a value only
   * if the codec can decode it into something which is `equal` to the
   * object. */
  implicit def litifyDecode[T : JsonDecode](x: T): Pattern =
    new FLiteral(j => JsonDecode[T].decode(j) == Right(x)) with ReallyDecode[T] {
      override def generate(env: Pattern.Results) =
        None
      def decode = JsonDecode[T]
    }
}
