package com.rojoma.json.v3
package codec

import scala.{collection => sc}
import scala.language.implicitConversions

import ast._

sealed trait DecodeError {
  def augment(parent: Path.Entry): DecodeError
  def english: String
}

object DecodeError {
  // can't be added to DecodeError as
  //    def augment(parent: String/Int): DecodeError = augment(parent)
  // for binary compat reasons, and can't keep the name "augment" because
  // the existing augment will hide it from implicit search.  So it's
  // called "prefix"!
  implicit def prefixable(de: DecodeError) = new `-impl`.codec.PrefixableDecodeError(de)

  /** There were several choices and they all failed. */
  case class Multiple(choices: Seq[Simple]) extends DecodeError {
    def augment(parent: Path.Entry) = copy(choices = choices.map(_.augment(parent)))
    def english = choices.map(_.english).mkString(" OR ")
  }

  def join(choices: Iterable[DecodeError]): DecodeError = {
    // we want to keep only the longest choices as they are most
    // likely to be useful.
    val selectedChoices =
      if(choices.isEmpty) { // shouldn't happen, but don't crash if it does
        choices
      } else {
        val maxLen = choices.iterator.map {
          case simple: Simple =>
            simple.path.toList.length // I should've made Path cache its own length...
          case Multiple(subchoices) =>
            if(subchoices.isEmpty) 0 // shouldn't happen, but don't crash if it does
            else subchoices.iterator.map(_.path.toList.length).max
          }.max
        choices.filter {
          case simple: Simple =>
            simple.path.toList.length == maxLen
          case Multiple(subchoices) =>
            if(subchoices.isEmpty) maxLen == 0
            else subchoices.exists(_.path.toList.length == maxLen)
        }
      }

    val deduped =
      if(selectedChoices.isInstanceOf[sc.Set[_]]) selectedChoices
      else new sc.mutable.LinkedHashSet ++ selectedChoices

    if(deduped.size == 1) selectedChoices.iterator.next()
    else Multiple(deduped.toSeq.flatMap {
                    case Multiple(subchoices) => subchoices
                    case simple: Simple => Seq(simple)
                  })
  }

  sealed abstract class Simple extends DecodeError {
    val path: Path
    def augment(parent: Path.Entry): Simple
  }

  /** A value was found in the correct position but of the wrong type. */
  case class InvalidType(expected: JsonType, got: JsonType, path: Path = Path.empty) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid type at " + path + ": expected " + expected + "; got " + got
  }

  /** A value of the correct JSON type was found but it held undecodable value. */
  case class InvalidValue(got: JValue, path: Path = Path.empty) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid value at " + path + ": got " + got
  }

  /** A required field was missing. */
  case class MissingField(field: String, path: Path = Path.empty) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Missing field at " + path + ": expected " + JString(field)
  }

  /** An unknown or uninterpretable field was present. Stock rojoma-json
    * decoders only return this for maps whose keys are rejected by
    * the `FieldDecode`, but user codecs can use it to reject unknown
    * fields if desired. */
  case class InvalidField(field: String, path: Path = Path.empty) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Unexpected field at " + path + ": got " + JString(field)
  }

  /** An array with the wrong number of elements was found. */
  case class InvalidLength(expected: Int, got: Int, path: Path = Path.empty) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid length at " + path + ": expected " + expected + "; got " + got
  }

  implicit val jCodec: JsonEncode[DecodeError] with JsonDecode[DecodeError] = new JsonEncode[DecodeError] with JsonDecode[DecodeError] {
    import matcher._
    import util._

    // We can't use SimpleJsonCodecBuilder for the sub-codecs because
    // it doesn't play nicely with value class fields like `path'.

    private implicit val itCodec = new JsonEncode[InvalidType] with JsonDecode[InvalidType] {
      private val expected = Variable[JsonType]()
      private val got = Variable[JsonType]()
      private val path = Variable[Path]()

      private val itPattern = PObject(
        "expected" -> expected,
        "got" -> got,
        "path" -> path)

      def encode(v: InvalidType) = itPattern.generate(expected := v.expected, got := v.got, path := v.path)
      def decode(v: JValue) = itPattern.matches(v).right.map { results =>
        InvalidType(expected = expected(results), got = got(results), path(results))
      }
    }


    private implicit val ivCodec = new JsonEncode[InvalidValue] with JsonDecode[InvalidValue] {
      private val got = Variable[JValue]()
      private val path = Variable[Path]()

      private val ivPattern = PObject(
        "got" -> got,
        "path" -> path)

      def encode(v: InvalidValue) = ivPattern.generate(got := v.got, path := v.path)
      def decode(v: JValue) = ivPattern.matches(v).right.map { results =>
        InvalidValue(got(results), path(results))
      }
    }

    private implicit val mfCodec = new JsonEncode[MissingField] with JsonDecode[MissingField] {
      private val field = Variable[String]()
      private val path = Variable[Path]()

      private val mfPattern = PObject(
        "field" -> field,
        "path" -> path)

      def encode(v: MissingField) = mfPattern.generate(field := v.field, path := v.path)
      def decode(v: JValue) = mfPattern.matches(v).right.map { results =>
        MissingField(field(results), path(results))
      }
    }

    private implicit val ifCodec = new JsonEncode[InvalidField] with JsonDecode[InvalidField] {
      private val field = Variable[String]()
      private val path = Variable[Path]()

      private val ifPattern = PObject(
        "field" -> field,
        "path" -> path)

      def encode(v: InvalidField) = ifPattern.generate(field := v.field, path := v.path)
      def decode(v: JValue) = ifPattern.matches(v).right.map { results =>
        InvalidField(field(results), path(results))
      }
    }

    private implicit val ilCodec = new JsonEncode[InvalidLength] with JsonDecode[InvalidLength] {
      private val expected = Variable[Int]()
      private val got = Variable[Int]()
      private val path = Variable[Path]()

      private val ilPattern = PObject(
        "expected" -> expected,
        "got" -> got,
        "path" -> path)

      def encode(v: InvalidLength) = ilPattern.generate(expected := v.expected, got := v.got, path := v.path)
      def decode(v: JValue) = ilPattern.matches(v).right.map { results =>
        InvalidLength(expected = expected(results), got = got(results), path(results))
      }
    }

    private implicit val simpleCodec =
      SimpleHierarchyCodecBuilder[Simple](InternalTag("type", false)).
        branch[InvalidType]("invalid_type").
        branch[InvalidValue]("invalid_value").
        branch[MissingField]("missing_field").
        branch[InvalidField]("invalid_field").
        branch[InvalidLength]("invalid_length").
        build

    private val anyDecode = JsonDecode[Either[Seq[Simple], Simple]]

    def encode(x: DecodeError) = x match {
      case Multiple(choice) => JsonEncode.toJValue(choice)
      case s: Simple => simpleCodec.encode(s)
    }

    def decode(v: JValue) = anyDecode.decode(v).right.map {
      case Right(e) => e
      case Left(es) => Multiple(es)
    }
  }
}
