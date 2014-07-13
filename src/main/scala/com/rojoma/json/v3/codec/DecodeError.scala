package com.rojoma.json.v3
package codec

import scala.{collection => sc}

import ast._

sealed trait DecodeError {
  def augment(parent: Path.Entry): DecodeError
  def english: String
}

object DecodeError {
  /** There were several choices and they all failed. */
  case class Multiple(choices: Seq[Simple]) extends DecodeError {
    def augment(parent: Path.Entry) = copy(choices = choices.map(_.augment(parent)))
    def english = choices.map(_.english).mkString(" OR ")
  }

  def join(choices: Iterable[DecodeError]): DecodeError = {
    val deduped =
      if(choices.isInstanceOf[sc.Set[_]]) choices
      else new sc.mutable.LinkedHashSet ++ choices
    if(deduped.size == 1) choices.iterator.next()
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
  case class InvalidType(expected: JsonType, got: JsonType, path: Path) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid type at " + path + ": expected " + expected + "; got " + got
  }

  /** A value of the correct JSON type was found but it held undecodable value. */
  case class InvalidValue(got: JValue, path: Path) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid value at " + path + ": got " + got
  }

  /** A required field was missing. */
  case class MissingField(field: String, path: Path) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Missing field at " + path + ": expected " + JString(field)
  }

  /** An unknown field was present. */
  case class InvalidField(field: String, path: Path) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Unexpected field at " + path + ": got " + JString(field)
  }

  /** An array with the wrong number of elements was found. */
  case class InvalidLength(expected: Int, got: Int, path: Path) extends Simple {
    def augment(parent: Path.Entry) = copy(path = path.prepend(parent))
    def english = "Invalid length at " + path + ": expected " + expected + "; got " + got
  }
}
