package com.rojoma.json.v3
package `-impl`.codec

import codec.Path

sealed trait EntryLike {
  def toEntry: Path.Entry
}

object EntryLike {
  given Conversion[String, EntryLike] with
    def apply(s: String) = FieldEntryLike(s)

  given Conversion[Int, EntryLike] with
    def apply(i: Int) = IndexEntryLike(i)
}

case class FieldEntryLike(s: String) extends EntryLike {
  def toEntry = Path.Field(s)
}

case class IndexEntryLike(i: Int) extends EntryLike {
  def toEntry = Path.Index(i)
}
