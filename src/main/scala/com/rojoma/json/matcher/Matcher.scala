package com.rojoma.json
package matcher

import ast._
import codec.JsonCodec

sealed trait OptPattern

object OptPattern {
  implicit def litifyCodec[T : JsonCodec](x: T): Pattern = FLiteral(j => implicitly[JsonCodec[T]].decode(j) == Some(x))
  implicit def litifyLong(x: Long): Pattern = Literal(JNumber(x))
  implicit def litifyDouble(x: Double): Pattern = Literal(JNumber(x))
}

trait Pattern extends OptPattern {
  def matches(x: JValue) = evaluate(x, Map.empty)
  def evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results]

  def unapply(x: JValue) = matches(x)
}

object Pattern {
  type Results = Map[Variable[_], Any]

  private[matcher] def foldLeftOpt[A, B](seq: Iterable[B], init: A)(f: (A, B) => Option[A]): Option[A] = {
    val it = seq.iterator
    var acc = init
    while(it.hasNext) {
      f(acc, it.next()) match {
        case None => return None
        case Some(r) => acc = r
      }
    }
    return Some(acc)
  }
}

case class Literal(literal: JValue) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    if(x == literal) Some(environment)
    else None
}

case class FLiteral(recognizer: JValue => Boolean) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    if(recognizer(x)) Some(environment)
    else None
}

abstract class Variable[+T] extends Pattern with PartialFunction[Pattern.Results, T] {
  def apply(results: Pattern.Results): T =
    results(this).asInstanceOf[T]

  def get(results: Pattern.Results): Option[T] =
    results.get(this).map(_.asInstanceOf[T])

  def getOrElse[U >: T](results: Pattern.Results, alternative: => U): U =
    results.get(this).map(_.asInstanceOf[T]).getOrElse(alternative)

  def isDefinedAt(results: Pattern.Results) = results.isDefinedAt(this)

  def isBound(results: Pattern.Results) = isDefinedAt(results)
}

object Variable {
  def apply[T : JsonCodec](): Variable[T] = new Variable[T] {
    def evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results] = {
      implicitly[JsonCodec[T]].decode(x) flatMap { r1 =>
        environment.get(this) match {
          case None =>
            Some(environment + (this -> r1))
          case Some(r2) if r2 == r1 =>
            Some(environment)
          case _ =>
            None
        }
      }
    }
  }
}
case class PArray(subPatterns: Pattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    x.cast[JArray] flatMap { arr =>
      if(arr.length < subPatterns.length) {
        None
      } else {
        Pattern.foldLeftOpt(arr zip subPatterns, environment) { (env, vp) =>
          val (subValue, subPattern) = vp
          subPattern.evaluate(subValue, env)
        }
      }
    }
}

case class PObject(subPatterns: (String, OptPattern)*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    x.cast[JObject] flatMap { obj =>
      Pattern.foldLeftOpt(subPatterns, environment) { (env, sp) =>
        sp match {
          case (subKey, subPat: Pattern) =>
            obj.get(subKey) match {
              case Some(subValue) =>
                subPat.evaluate(subValue, env)
              case None =>
                None
            }
          case (subKey, POption(subPat)) =>
            obj.get(subKey) match {
              case Some(subValue) =>
                subPat.evaluate(subValue, env)
              case None =>
                Some(env)
            }
        }
      }
    }
}

case class FirstOf(subPatterns: Pattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) = {
    val it = subPatterns.iterator
    def loop(): Option[Pattern.Results] = {
      if(!it.hasNext) None
      else it.next().evaluate(x, environment) match {
        case None => loop()
        case res => res
      }
    }
    loop()
  }
}

case class AllOf(subPatterns: OptPattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    Pattern.foldLeftOpt(subPatterns, environment) { (env, subPat) =>
      subPat match {
        case pat: Pattern =>
          pat.evaluate(x, env)
        case POption(pat) =>
          pat.evaluate(x, env) orElse Some(env)
      }
    }
}

case class POption(subPattern: Pattern) extends OptPattern
