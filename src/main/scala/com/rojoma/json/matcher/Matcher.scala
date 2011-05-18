package com.rojoma.json
package matcher

import ast._
import codec.JsonCodec

sealed trait OptPattern

sealed trait Pattern extends OptPattern {
  def matches(x: JValue) = Pattern.matches(x, this, Map.empty[Variable[_], AnyRef])
  def unapply(x: JValue) = matches(x)
}
object Pattern {
  implicit def litify[T : JsonCodec](x: T): Pattern = FLiteral(j => implicitly[JsonCodec[T]].decode(j) == Some(x))
  implicit def litify(x: JValue): Pattern = Literal(x)
  implicit def litify(x: Long): Pattern = Literal(JNumber(x))
  implicit def litify(x: Double): Pattern = Literal(JNumber(x))
  implicit def litify(x: String): Pattern = Literal(JString(x))
  implicit def litify(x: Boolean): Pattern = Literal(JBoolean(x))

  type Results = Map[Variable[_], Any]

  private [matcher] def matches(x: JValue, pattern: OptPattern, environment: Results): Option[Results] = pattern match {
    case Literal(atom: JAtom) =>
      if(x == atom) Some(environment)
      else None
    case Literal(pat: JArray) => // matches if x is a sequence and pat is a prefix of that sequence
      x.cast[JArray] flatMap { lit =>
        if(pat.length <= lit.length && pat.zip(lit).forall { case (a,b) => a == b }) Some(environment)
        else None
      }
    case Literal(pat: JObject) => // matches if x is an object and pat is a subset of that object
      x.cast[JObject] flatMap { lit =>
        if(pat.forall { case (k, v) => lit.get(k) == Some(v) }) Some(environment)
        else None
      }
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
          arr.zip(subPatterns).foldLeft(Some(environment) : Option[Results]) { (env, vp) =>
            env match {
              case None => None
              case Some(env) =>
                val (value,pattern) = vp
                matches(value, pattern, env)
            }
          }
        }
      }
    case PObject(subPatterns @ _*) =>
      x.cast[JObject] flatMap { obj =>
        subPatterns.foldLeft(Some(environment) : Option[Results]) { (env, sp) =>
          env match {
            case None => None
            case Some(env) =>
              val (k, v) = sp
              if(obj contains k) {
                matches(obj(k), v, env)
              } else {
                v match {
                  case _: Pattern =>
                    None
                  case _: POption =>
                    Some(environment)
                }
              }
          }
        }
      }
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

case class POption(subPattern: Pattern) extends OptPattern
