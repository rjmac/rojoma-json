package com.rojoma.json.v3
package matcher

import scala.language.implicitConversions

import ast._
import codec._

import `-impl`.matcher._

class JsonGenerationException extends RuntimeException("Cannot generate JSON; this is always a logic error.  You've forgotten to bind a variable, or you've used a pattern that cannot generate.")

/** Either a [[com.rojoma.json.v3.matcher.Pattern]] or a [[com.rojoma.json.v3.matcher.POption]]. */
sealed trait OptPattern

object OptPattern extends LowPriorityImplicits {
  implicit def litifyJValue(x: JValue): Pattern = x match {
    case atom: JAtom => Literal(atom)
    case JArray(arr) => PArray(arr.map(litifyJValue) : _*)
    case JObject(obj) => PObject(obj.mapValues(litifyJValue).toSeq : _*)
  }

  /** Converts an object with a [[com.rojoma.json.v3.codec.JsonDecode]]
   * and [[com.rojoma.json.v3.codec.JsonEncode]] into a
   * [[com.rojoma.json.v3.matcher.Pattern]] which matches a value only
   * if the codec can decode it into something which is `equal` to the
   * object. */
  implicit def litifyCodec[T : JsonDecode : JsonEncode](lit: T): Pattern =
    new Pattern {
      def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
        JsonDecode[T].decode(x) match {
          case Right(y) if lit == y => Right(environment)
          case Right(_) => Left(DecodeError.InvalidValue(x))
          case Left(err) => Left(err)
        }

      def generate(environment: Pattern.Results): Option[JValue] = Some(JsonEncode[T].encode(lit))
    }
}

/** An object that can be used to either match and extract data from,
 * or generate, [[com.rojoma.json.v3.ast.JValue]]s. */
trait Pattern extends OptPattern {
  /** Tests the given [[com.rojoma.json.v3.ast.JValue]] against this `Pattern`,
   * and if it matches returns an object that can be used to retrieve the
   * values matched by any [[com.rojoma.json.v3.matcher.Variable]]s in the
   * `Pattern`.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val strVar = Variable[String]
   * val pattern = PObject("i" -> intVar, "s" -> strVar)
   *
   * pattern.matches(jvalue) match {
   *   case Right(results) => println("The integer was " + intVar(results))
   *   case Left(_) => println("It didn't match the pattern")
   * }
   * }}}
   *
   * @param x The value to test.
   * @return An environment which can be used to look up variable bindings, or `Left(error)` if it didn't match.
   */
  def matches(x: JValue) = evaluate(x, Map.empty)

  /** Tests the given [[com.rojoma.json.v3.ast.JValue]] against this `Pattern`, with the
   * restriction that any [[com.rojoma.json.v3.matcher.Variable]]s that are bound in the
   * `environment` must match those values if they are re-used in this `Pattern`.
   *
   * Generally you won't use this directly, but you can if you want to pre-populate variables.
   *
   * @param x The value to test.
   * @return The environment augmented with any new [[com.rojoma.json.v3.matcher.Variable]]s
   *    encountered in this `Pattern`, or `Left(error)` if it didn't match. */
  def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results]

  /** Uses this `Pattern` together with the provided variable bindings to generate a
   * new [[com.rojoma.json.v3.ast.JValue]].
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val strVar = Variable[String]
   * val pattern = PObject("i" -> intVar, "s" -> POption(strVar))
   *
   * pattern.generate(i := 1)                      // { "i" : 1 }
   * pattern.generate(i := 1, s := "hello")        // { "i" : 1, "s" : "hello" }
   * pattern.generate(i := 1, s :=? None )         // { "i" : 1 }
   * pattern.generate(i := 1, s :=? Some("hello")) // { "i" : 1, "s" : "hello" }
   * }}}
   *
   * @return The new [[com.rojoma.json.v3.ast.JValue]]
   * @throws JsonGenerationException if a required [[com.rojoma.json.v3.matcher.Variable]]
   *   is not bound or a matcher which cannot generate (such as [[com.rojoma.json.v3.matcher.AllOf]])
   *   is used.
   */
  def generate(bindings: (Pattern.Results => Pattern.Results)*): JValue =
    generate(bindings.foldLeft(Map.empty : Pattern.Results) { (e, b) => b(e) }).getOrElse(throw new JsonGenerationException)

  /** Uses this `Pattern` together with the bindings generated as the result of a
   * call to `matches` or `evaluate` to produce a [[com.rojoma.json.v3.ast.JValue]].
   *
   * Generally the other `generate` method is simpler to use.
   *
   * @return The new [[com.rojoma.json.v3.ast.JValue]], or None if a required
   *   [[com.rojoma.json.v3.matcher.Variable]] is not bound in the environment,
   *   or a matcher which cannot generate is used.
   */
  def generate(environment: Pattern.Results): Option[JValue]

  /** Allows this `Pattern` to be used in a `match` expression, with the output
   * being the environment of [[com.rojoma.json.v3.matcher.Variable]] bindings
   * as produced by `matches`.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val strVar = Variable[String]
   * val Pattern1 = PObject("i" -> intVar, "s" -> strVar)
   * val Pattern2 = PObject("hello" -> "world")
   *
   * jvalue match {
   *   case Pattern1(results) => println("The integer was " + intVar(results))
   *   case Pattern2(result) => println("It was just a hello world object")
   *   case _ => println("It was something else")
   * }
   * }}}
   */
  def unapply(x: JValue): Option[Pattern.Results] = matches(x).right.toOption
}

object Pattern {
  type Results = Map[Variable[_], Any]

  private[matcher] def foldMatches[A, B](seq: Iterable[B], init: A)(f: (A, B, Int) => Either[DecodeError, A]): Either[DecodeError, A] = {
    val it = seq.iterator
    var i = 0
    var acc = init
    while(it.hasNext) {
      f(acc, it.next(), i) match {
        case Right(r) => acc = r
        case err@Left(_) => return err
      }
      i += 1
    }
    return Right(acc)
  }

  private[matcher] def foldLeftOpt[A, B](seq: Iterable[B], init: A)(f: (A, B) => Option[A]): Option[A] = {
    val it = seq.iterator
    var acc = init
    while(it.hasNext) {
      f(acc, it.next()) match {
        case Some(r) => acc = r
        case None => return None
      }
    }
    return Some(acc)
  }
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches a [[com.rojoma.json.v3.ast.JValue]]
 * exactly.  Generally this is not used explicitly; [[com.rojoma.json.v3.ast.JValue]]s
 * can be implicitly converted into `Literal`s. */
case class Literal(literal: JValue) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
    if(x == literal) Right(environment)
    else if(x.jsonType == literal.jsonType) Left(DecodeError.InvalidValue(x))
    else Left(DecodeError.InvalidType(literal.jsonType, x.jsonType))

  def generate(environment: Pattern.Results) = Some(literal)
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches a [[com.rojoma.json.v3.ast.JValue]]
 * if a predicate on that JValue is true.  This `Pattern` cannot be used to generate a
 * [[com.rojoma.json.v3.ast.JValue]]. */
case class FLiteral(recognizer: JValue => Boolean) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
    if(recognizer(x)) Right(environment)
    else Left(DecodeError.InvalidValue(x))

  def generate(environment: Pattern.Results): Option[JValue] = None
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches any [[com.rojoma.json.v3.ast.JValue]]
 * which can be decoded into an object of type `T`.  If this variable is
 * already bound in the environment at match-time, then this matches only if
 * the two decoded objects are `equal`, in which case the environment is
 * unchanged. */
abstract class Variable[T] extends Pattern with PartialFunction[Pattern.Results, T] {
  /** Look up the value of this variable in an environment.
   *
   * @return The value found
   * @throws NoSuchElementException if the variable is not bound. */
  def apply(results: Pattern.Results): T =
    results(this).asInstanceOf[T]

  /** Look up the value of this variable in an environment.
   *
   * @return The value found, or None if it was not bound. */
  def get(results: Pattern.Results): Option[T] =
    results.get(this).map(_.asInstanceOf[T])

  /** Look up the value of this variable in an environment.
   *
   * @return The value found, or `alternative` if it was not bound. */
  def getOrElse[U >: T](results: Pattern.Results, alternative: => U): U =
    results.get(this).map(_.asInstanceOf[T]).getOrElse(alternative)

  def isDefinedAt(results: Pattern.Results) = results.isDefinedAt(this)

  def isBound(results: Pattern.Results) = isDefinedAt(results)

  /** Bind this variable into an environment.  This is usually used with
   * [[com.rojoma.json.v3.matcher.Pattern]]#generate.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val pattern = PObject("i" -> intVar)
   *
   * println(pattern.generate(intVar := 5)) // { "i" : 5 }
   * }}} */
  def := (x: T): Pattern.Results => Pattern.Results = _ + (this -> x)

  /** Possibly this variable into an environment.  This is usually used with
   * [[com.rojoma.json.v3.matcher.Pattern]]#generate.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val pattern = PObject("i" -> POption(intVar))
   *
   * println(pattern.generate(intVar :=? Some(5))) // { "i" : 5 }
   * println(pattern.generate(intVar :=? None)) // { }
   * }}} */
  def :=? (x: Option[T]): Pattern.Results => Pattern.Results = x match {
    case Some(x) => this := x
    case None => identity
  }
}

object Variable {
  private abstract class DecodingVariable[T](dec: JsonDecode[T]) extends Variable[T] {
    def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] = {
      dec.decode(x) match {
        case Right(r1) =>
          environment.get(this) match {
            case None =>
              Right(environment + (this -> r1))
            case Some(r2) if r2 == r1 =>
              Right(environment)
            case _ =>
              Left(DecodeError.InvalidValue(x))
          }
        case Left(err) =>
          Left(err)
      }
    }
  }

  def apply[T : JsonDecode : JsonEncode](): Variable[T] =
    new DecodingVariable(JsonDecode[T]) {
      def generate(environment: Pattern.Results) =
        get(environment).map(JsonEncode[T].encode)
    }

  def decodeOnly[T : JsonDecode]: Variable[T] =
    new DecodingVariable(JsonDecode[T]) {
      def generate(environment: Pattern.Results) =
        None
    }
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches if the value is
 * a [[com.rojoma.json.v3.ast.JArray]] which contains at least as many elements
 * as sub-patterns contained by this object and those elements match the
 * sub-patterns in the order given.
 *
 * For the more common case where a sequence of repeated objects of a
 * particular type is desired, use a [[com.rojoma.json.v3.matcher.Variable]]
 * of some `Seq[T]`.
 */
case class PArray(subPatterns: Pattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
    x match {
      case arr: JArray =>
        if(arr.length != subPatterns.length) {
          Left(DecodeError.InvalidLength(subPatterns.length, arr.length))
        } else {
          Pattern.foldMatches(arr zip subPatterns, environment) { (env, vp, i) =>
            val (subValue, subPattern) = vp
            subPattern.evaluate(subValue, env) match {
              case r@Right(_) => r
              case Left(err) => return Left(err.prefix(i))
            }
          }
        }
      case other =>
        Left(DecodeError.InvalidType(JArray, other.jsonType))
    }

  def generate(environment: Pattern.Results) = {
    val subValues = subPatterns.map(_.generate(environment))
    if(subValues.forall(_.isDefined))
      Some(JArray(subValues.map(_.get)))
    else
      None
  }
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches if the value is
 * a [[com.rojoma.json.v3.ast.JObject]] which contains at least the fields
 * specified in this `Pattern`.  In order to allow fields to not appear
 * at all, the sub-patterns can be wrapped in a [[com.rojoma.json.v3.matcher.POption]].
 *
 * @example {{{
 *   val i = Variable[Int]
 *   val s = Variable[String]
 *   val b = Variable[Boolean]
 *   val f = Variable[Float]
 *   val pattern = PObject(
 *     "i" -> i,                 // must be present
 *     "s" -> POption(s),        // may be absent but must not be null
 *     "b" -> POption(b).orNull, // may be present, absent, or null
 *     "f" -> FirstOf(f, JNull)  // must be present but may be null
 *   )
 *
 *   pattern.matches(jvalue) match {
 *     case Some(results) =>
 *       println("i: " + i(results))
 *       println("s: " + s.getOrElse(results, "[not present]"))
 *       println("b: " + b.getOrElse(results, "[not present or null]"))
 *       println("f: " + f.getOrElse(results, "[null]"))
 *    case None =>
 *       println("Didn't match")
 *   }
 * }}}
 */
case class PObject(subPatterns: (String, OptPattern)*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) = x match {
    case obj: JObject =>
      Pattern.foldMatches(subPatterns, environment) { (env, sp, _) =>
        sp match {
          case (subKey, subPat: Pattern) =>
            obj.get(subKey) match {
              case Some(subValue) =>
                subPat.evaluate(subValue, env) match {
                  case r@Right(_) => r
                  case Left(err) => Left(err.prefix(subKey))
                }
              case None =>
                Left(DecodeError.MissingField(subKey))
            }
          case (subKey, POption(subPat)) =>
            obj.get(subKey) match {
              case Some(subValue) =>
                subPat.evaluate(subValue, env) match {
                  case r@Right(_) => r
                  case Left(err) => Left(err.prefix(subKey))
                }
              case None =>
                Right(env)
            }
        }
      }
    case other =>
      Left(DecodeError.InvalidType(JObject, other.jsonType))
  }


  def generate(environment: Pattern.Results) = {
    val newObject = Pattern.foldLeftOpt(subPatterns, Map.empty[String, JValue]) { (result, sp) =>
      def require(subKey: String, subPat: Pattern) = {
        subPat.generate(environment) match {
          case Some(subValue) =>
            Some(result + (subKey -> subValue))
          case None =>
            None
        }
      }
      def permit(subKey: String, subPat: Pattern) = {
        subPat.generate(environment) match {
          case Some(subValue) =>
            Some(result + (subKey -> subValue))
          case None =>
            Some(result)
        }
      }
      sp match {
        case (subKey, subPat: Pattern) => require(subKey, subPat)
        case (subKey, POption(FirstOf(subPat, Literal(JNull)))) => permit(subKey, subPat)
        case (subKey, POption(subPat)) => permit(subKey, subPat)
      }
    }
    newObject.map(JObject)
  }
}

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches the first
 * sub-pattern to succeed.  This is frequently used to allow a
 * value to be something-or-`null`. */
case class FirstOf(subPatterns: Pattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) = {
    val it = subPatterns.iterator
    def loop(fails: List[DecodeError]): Either[DecodeError, Pattern.Results] = {
      if(!it.hasNext) Left(DecodeError.join(fails.reverse))
      else it.next().evaluate(x, environment) match {
        case Right(res) => Right(res)
        case Left(err) => loop(err :: fails)
      }
    }
    loop(Nil)
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

/** A [[com.rojoma.json.v3.matcher.Pattern]] which matches only if
 * all the sub-patterns also match.  This pattern cannot be
 * used to `generate` a [[com.rojoma.json.v3.ast.JValue]]. */
case class AllOf(subPatterns: OptPattern*) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results): Either[DecodeError, Pattern.Results] =
    Pattern.foldMatches(subPatterns, environment) { (env, subPat, _) =>
      subPat match {
        case pat: Pattern =>
          pat.evaluate(x, env) // No need to augment
        case POption(pat) =>
          pat.evaluate(x, env) match {
            case Right(auged) => Right(auged)
            case Left(_) => Right(env)
          }
      }
    }

  def generate(environment: Pattern.Results) = None
}

/** A wrapper for a [[com.rojoma.json.v3.matcher.Pattern]] which allows
 * fields to be absent when using a [[com.rojoma.json.v3.matcher.PObject]].
 *
 * @see [[com.rojoma.json.v3.matcher.PObject]] */
case class POption(subPattern: Pattern) extends OptPattern {
  /** Shorthand to allow a value to be present, absent, or `null`, with the
   * absent and null cases considered equivalent.  During generation,
   * this will produce no field at all if the subpattern cannot generate a
   * value. */
  def orNull = POption(FirstOf(subPattern, Literal(JNull)))
}
