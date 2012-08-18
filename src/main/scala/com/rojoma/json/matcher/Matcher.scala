package com.rojoma.json
package matcher

import ast._
import codec.JsonCodec

class JsonGenerationException extends RuntimeException("Cannot generate JSON; this is always a logic error.  You've forgotten to bind a variable, or you've used a pattern that cannot generate.")

/** Either a [[com.rojoma.json.Pattern]] or a [[com.rojoma.json.POption]]. */
sealed trait OptPattern

object OptPattern {
  implicit def litifyJValue(x: JValue): Pattern = Literal(x)

  /** Converts an object with a [[com.rojoma.json.codec.JsonCodec]] into a
   * [[com.rojoma.json.matcher.Pattern]] which matches a value only if the
   * codec can decode it into something which is `equal` to the object. */
  implicit def litifyCodec[T : JsonCodec](x: T): Pattern = new FLiteral(j => JsonCodec[T].decode(j) == Some(x)) {
    override def generate(env: Pattern.Results) = Some(JsonCodec[T].encode(x))
  }
}

/** An object that can be used to either match and extract data from,
 * or generate, [[com.rojoma.json.ast.JValue]]s. */
trait Pattern extends OptPattern {
  /** Tests the given [[com.rojoma.json.ast.JValue]] against this `Pattern`,
   * and if it matches returns an object that can be used to retrieve the
   * values matched by any [[com.rojoma.json.matcher.Variable]]s in the
   * `Pattern`.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val strVar = Variable[String]
   * val pattern = PObject("i" -> intVar, "s" -> strVar)
   *
   * pattern.matches(jvalue) match {
   *   case Some(results) => println("The integer was " + intVar(results))
   *   case None => println("It didn't match the pattern")
   * }
   * }}}
   *
   * @param x The value to test.
   * @return An environment which can be used to look up variable bindings, or `None` if it didn't match.
   */
  def matches(x: JValue) = evaluate(x, Map.empty)

  /** Tests the given [[com.rojoma.json.ast.JValue]] against this `Pattern`, with the
   * restriction that any [[com.rojoma.json.matcher.Variable]]s that are bound in the
   * `environment` must match those values if they are re-used in this `Pattern`.
   *
   * Generally you won't use this directly.
   *
   * @param x The value to test.
   * @return The environment augmented with any new [[com.rojoma.json.matcher.Variable]]s
   *    encountered in this `Pattern`, or `None` if it didn't match. */
  def evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results]

  /** Uses this `Pattern` together with the provided variable bindings to generate a
   * new [[com.rojoma.json.ast.JValue]].
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
   * @return The new [[com.rojoma.json.ast.JValue]]
   * @throws JsonGenerationException if a required [[com.rojoma.json.matcher.Variable]]
   *   is not bound or a matcher which cannot generate (such as [[com.rojoma.json.matcher.AllOf]])
   *   is used.
   */
  def generate(bindings: (Pattern.Results => Pattern.Results)*): JValue =
    generate(bindings.foldLeft(Map.empty : Pattern.Results) { (e, b) => b(e) }).getOrElse(throw new JsonGenerationException)

  /** Uses this `Pattern` together with the bindings generated as the result of a
   * call to `matches` or `evaluate` to produce a [[com.rojoma.json.ast.JValue]].
   *
   * Generally the other `generate` method is simpler to use.
   *
   * @return The new [[com.rojoma.json.ast.JValue]], or None if a required
   *   [[com.rojoma.json.matcher.Variable]] is not bound in the environment,
   *   or a matcher which cannot generate is used.
   */
  def generate(environment: Pattern.Results): Option[JValue]

  /** Allows this `Pattern` to be used in a `match` expression, with the output
   * being the environment of [[com.rojoma.json.matcher.Variable]] bindings
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

/** A [[com.rojoma.json.matcher.Pattern]] which matches a [[com.rojoma.json.ast.JValue]]
 * exactly.  Generally this is not used explicitly; [[com.rojoma.json.ast.JValue]]s
 * can be implicitly converted into `Literal`s. */
case class Literal(literal: JValue) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    if(x == literal) Some(environment)
    else None

  def generate(environment: Pattern.Results) = Some(literal)
}

/** A [[com.rojoma.json.matcher.Pattern]] which matches a [[com.rojoma.json.ast.JValue]]
 * if a predicate on that JValue is true.  This `Pattern` cannot be used to generate a
 * [[com.rojoma.json.ast.JValue]]. */
case class FLiteral(recognizer: JValue => Boolean) extends Pattern {
  def evaluate(x: JValue, environment: Pattern.Results) =
    if(recognizer(x)) Some(environment)
    else None

  def generate(environment: Pattern.Results): Option[JValue] = None
}

/** A [[com.rojoma.json.matcher.Pattern]] which matches any [[com.rojoma.json.ast.JValue]]
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
   * [[com.rojoma.json.matcher.Pattern]]#generate.
   *
   * @example {{{
   * val intVar = Variable[Int]
   * val pattern = PObject("i" -> intVar)
   *
   * println(pattern.generate(intVar := 5)) // { "i" : 5 }
   * }}} */
  def := (x: T): Pattern.Results => Pattern.Results = _ + (this -> x)

  /** Possibly this variable into an environment.  This is usually used with
   * [[com.rojoma.json.matcher.Pattern]]#generate.
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
  def apply[T : JsonCodec](): Variable[T] = new Variable[T] {
    def evaluate(x: JValue, environment: Pattern.Results): Option[Pattern.Results] = {
      JsonCodec[T].decode(x) flatMap { r1 =>
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

    def generate(environment: Pattern.Results) = get(environment).map(JsonCodec[T].encode)
  }
}

/** A [[com.rojoma.json.matcher.Pattern]] which matches if the value is
 * a [[com.rojoma.json.ast.JArray]] which contains at least as many elements
 * as sub-patterns contained by this object and those elements match the
 * sub-patterns in the order given.
 *
 * For the more common case where a sequence of repeated objects of a
 * particular type is desired, use a [[com.rojoma.json.matcher.Variable]]
 * of some `Seq[T]`.
 */
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

/** A [[com.rojoma.json.matcher.Pattern]] which matches if the value is
 * a [[com.rojoma.json.ast.JObject]] which contains at least the fields
 * specified in this `Pattern`.  In order to allow fields to not appear
 * at all, the sub-patterns can be wrapped in a [[com.rojoma.json.matcher.POption]].
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

/** A [[com.rojoma.json.matcher.Pattern]] which matches the first
 * sub-pattern to succeed.  This is frequently used to allow a
 * value to be something-or-`null`. */
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

/** A [[com.rojoma.json.matcher.Pattern]] which matches only if
 * all the sub-patterns also match.  This pattern cannot be
 * used to `generate` a [[com.rojoma.json.ast.JValue]]. */
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

/** A wrapper for a [[com.rojoma.json.matcher.Pattern]] which allows
 * fields to be absent when using a [[com.rojoma.json.matcher.PObject]].
 *
 * @see [[com.rojoma.json.matcher.PObject]] */
case class POption(subPattern: Pattern) extends OptPattern {
  /** Shorthand to allow a value to be present, absent, or `null`, with the
   * absent and null cases considered equivalent.  During generation,
   * this will produce no field at all if the subpattern cannot generate a
   * value. */
  def orNull = POption(FirstOf(subPattern, Literal(JNull)))
}

