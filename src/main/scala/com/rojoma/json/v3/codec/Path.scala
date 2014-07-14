package com.rojoma.json.v3
package codec

import ast.JString
import `-impl`.codec.EntryLike

// This doesn't feel like it belongs in "codec"

/** Representation of a particular JSON path */
class Path(val toList: List[Path.Entry]) extends AnyVal {
  override def toString = Path.asString(toList)

  def prepend(prefix: Path.Entry) = new Path(prefix :: toList)
}

object Path {
  val empty = new Path(Nil)

  def apply(entry: EntryLike*) = new Path(entry.map(_.toEntry).toList)

  sealed trait Entry
  case class Index(index: Int) extends Entry
  case class Field(field: String) extends Entry

  // produces a jq-style path spec
  def asString(xs: List[Entry]) = {
    val sb = new StringBuffer(".")

    def appendEntry(e: Entry, isFirst: Boolean) {
      e match {
        case Field(f) =>
          if(isSimple(f)) {
            if(isFirst) sb.append(f)
            else sb.append('.').append(f)
          } else {
            sb.append('[').append(JString(f)).append(']')
          }
        case Index(i) =>
          sb.append('[').append(i).append(']')
      }
    }

    xs match {
      case e :: es =>
        appendEntry(e, true)
        for(e <- es) appendEntry(e, false)
      case Nil =>
        // nothing
    }
    sb.toString
  }

  private def isSimple(f: String): Boolean =
    f.nonEmpty && Character.isJavaIdentifierStart(f.charAt(0)) && restIsPart(f)
  private def restIsPart(f: String): Boolean = {
    var i = 1
    while(i < f.length) {
      if(!Character.isJavaIdentifierPart(f.charAt(i))) return false
      i += 1
    }
    true
  }
}
