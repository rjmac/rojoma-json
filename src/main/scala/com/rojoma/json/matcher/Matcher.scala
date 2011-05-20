package com.rojoma.json
package matcher

import ast._
import codec.JsonCodec

sealed trait OptPattern

object OptPattern {
  implicit def litify[T : JsonCodec](x: T): Pattern = FLiteral(j => implicitly[JsonCodec[T]].decode(j) == Some(x))
  implicit def litify(x: JValue): Pattern = Literal(x)
  implicit def litify(x: Long): Pattern = Literal(JNumber(x))
  implicit def litify(x: Double): Pattern = Literal(JNumber(x))
  implicit def litify(x: String): Pattern = Literal(JString(x))
  implicit def litify(x: Boolean): Pattern = Literal(JBoolean(x))
}

sealed trait Pattern extends OptPattern {
  def matches(x: JValue) = Pattern.matches(x, this, Map.empty[Variable[_], AnyRef])
  def unapply(x: JValue) = matches(x)
}
object Pattern {
  type Results = Map[Variable[_], Any]

  private def foldLeftOpt[A, B](seq: Iterable[B], init: A)(f: (A, B) => Option[A]): Option[A] = {
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

  private [matcher] def matches(x: JValue, pattern: OptPattern, environment: Results): Option[Results] = pattern match {
    case Literal(lit) =>
      if(x == lit) Some(environment)
      else None
    case FLiteral(recognizer) =>
      if(recognizer(x)) Some(environment)
      else None
    case v: Variable[_] =>
      v.maybeFill(x, environment)
    case PArray(subPatterns @ _*) =>
      x.cast[JArray] flatMap { arr =>
        if(arr.length < subPatterns.length) {
          None
        } else {
          foldLeftOpt(arr zip subPatterns, environment) { (env, vp) =>
            val (subValue, subPattern) = vp
            matches(subValue, subPattern, env)
          }
        }
      }
    case PObject(subPatterns @ _*) =>
      x.cast[JObject] flatMap { obj =>
        foldLeftOpt(subPatterns, environment) { (env, sp) =>
          val (subKey, subPat) = sp
          obj.get(subKey) match {
            case Some(subValue) =>
              matches(subValue, subPat, env)
            case None =>
              subPat match {
                case _: Pattern =>
                  None
                case _: POption =>
                  Some(env)
              }
          }
        }
      }
    case FirstOf(subPatterns @ _*) =>
      val it = subPatterns.iterator
      def loop(): Option[Results] = {
        if(!it.hasNext) None
        else matches(x, it.next(), environment) match {
          case None => loop()
          case res => res
        }
      }
      loop()
    case POption(subPattern) =>
      matches(x, subPattern, environment)
  }
}

case class Literal(underlying: JValue) extends Pattern
case class FLiteral(x: JValue => Boolean) extends Pattern

sealed abstract class Variable[+T] extends Pattern {
  def apply(results: Pattern.Results): T =
    results(this).asInstanceOf[T]

  def get(results: Pattern.Results): Option[T] =
    results.get(this).map(_.asInstanceOf[T])

  private [matcher] def maybeFill(x: JValue, environment: Pattern.Results): Option[Pattern.Results]
}

private[matcher] final class JVariable[T <: JValue : ClassManifest] extends Variable[T] {
  def maybeFill(x: JValue, environment: Pattern.Results): Option[Pattern.Results] = {
    x.cast[T] match {
      case None =>
        None
      case Some(r1) =>
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

private[matcher] final class CVariable[T : JsonCodec] extends Variable[T] {
  def maybeFill(x: JValue, environment: Pattern.Results): Option[Pattern.Results] = {
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

object Variable {
  def raw[T <: JValue : ClassManifest](): Variable[T] = new JVariable[T]()
  def cooked[T : JsonCodec](): Variable[T] = new CVariable[T]
}
case class PArray(subPatterns: Pattern*) extends Pattern
case class PObject(subPatterns: (String, OptPattern)*) extends Pattern
case class FirstOf(subPatterns: Pattern*) extends Pattern

case class POption(subPattern: Pattern) extends OptPattern
