package com.rojoma.json
package matcher

import ast._
import codec.JsonCodec

case class JsonGenerationException() extends RuntimeException("Cannot generate JSON; this is always a logic error.  You've forgotten to bind a variable, or you've used a pattern that cannot generate.")

sealed trait OptPattern

object OptPattern {
  implicit def litifyCodec[T : JsonCodec](x: T): Pattern = new FLiteral(j => implicitly[JsonCodec[T]].decode(j) == Some(x)) {
    override def generate(env: Pattern.Results) = Some(implicitly[JsonCodec[T]].encode(x))
  }
}

trait Pattern extends OptPattern {
  def matches(x: JValue) = evaluate(x, Map.empty)
  def evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results]

  def generate(bindings: (Pattern.Results => Pattern.Results)*): JValue =
    generate(bindings.foldLeft(Map.empty : Pattern.Results) { (e, b) => b(e) }).getOrElse(throw JsonGenerationException())

  def generate(environment: Pattern.Results): Option[JValue]

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

  def generate(environment: Pattern.Results) = Some(literal)
}

case class FLiteral(recognizer: JValue => Boolean) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    if(recognizer(x)) Some(environment)
    else None

  def generate(environment: Pattern.Results): Option[JValue] = None
}

abstract class Variable[T] extends Pattern with PartialFunction[Pattern.Results, T] {
  def apply(results: Pattern.Results): T =
    results(this).asInstanceOf[T]

  def get(results: Pattern.Results): Option[T] =
    results.get(this).map(_.asInstanceOf[T])

  def getOrElse[U >: T](results: Pattern.Results, alternative: => U): U =
    results.get(this).map(_.asInstanceOf[T]).getOrElse(alternative)

  def isDefinedAt(results: Pattern.Results) = results.isDefinedAt(this)

  def isBound(results: Pattern.Results) = isDefinedAt(results)

  def := (x: T) = (v: Pattern.Results) => v + (this -> x)

  def :=? (x: Option[T]) = x match {
    case Some(x) => this := x
    case None => (v: Pattern.Results) => v
  }
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

    def generate(environment: Pattern.Results) = get(environment).map(implicitly[JsonCodec[T]].encode)
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

  def generate(environment: Pattern.Results) = {
    val subValues = subPatterns.map(_.generate(environment))
    if(subValues.forall(_.isDefined))
      Some(JArray(subValues.map(_.get)))
    else
      None
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


  def generate(environment: Pattern.Results) = {
    val newObject = Pattern.foldLeftOpt(subPatterns, Map.empty[String, JValue]) { (result, sp) =>
      sp match {
        case (subKey, subPat: Pattern) =>
          subPat.generate(environment) match {
            case Some(subValue) =>
              Some(result + (subKey -> subValue))
            case None =>
              None
          }
        case (subKey, POption(subPat)) =>
          subPat.generate(environment) match {
            case Some(subValue) =>
              Some(result + (subKey -> subValue))
            case None =>
              Some(result)
          }
      }
    }
    newObject.map(JObject)
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

  def generate(environment: Pattern.Results) = {
    val it = subPatterns.iterator
    def loop(): Option[JValue] = {
      if(!it.hasNext) None
      else it.next().generate(environment) match {
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

  def generate(environment: Pattern.Results) = None
}

case class POption(subPattern: Pattern) extends OptPattern
