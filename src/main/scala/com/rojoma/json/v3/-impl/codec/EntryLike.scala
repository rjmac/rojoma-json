package com.rojoma.json.v3
package `-impl`.codec

import scala.language.implicitConversions

import codec.Path

sealed trait EntryLike {
  def toEntry: Path.Entry
}

object EntryLike {
  implicit def str2entry(s: String) = FieldEntryLike(s)
  implicit def int2entry(i: Int) = IndexEntryLike(i)
}

case class FieldEntryLike(s: String) extends EntryLike {
  def toEntry = Path.Field(s)
}

case class IndexEntryLike(i: Int) extends EntryLike {
  def toEntry = Path.Index(i)
}
