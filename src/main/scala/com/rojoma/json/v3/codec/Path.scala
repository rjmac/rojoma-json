package com.rojoma.json.v3
package codec

import ast.JString

// This doesn't feel like it belongs in "codec"

/** Representation of a particular JSON path */
class Path(val toList: List[Path.Entry]) extends AnyVal {
  override def toString = Path.asString(toList)

  def prepend(entry: Path.Entry) = new Path(entry :: toList)
}

object Path {
  val empty = new Path(Nil)

  sealed trait Entry
  case class Index(index: Int) extends Entry
  case class Field(field: String) extends Entry

  def asString(xs: Seq[Entry]) = {
    val sb = new StringBuffer()
    xs.foreach {
      case Field(f) =>
        if(isSimple(f)) sb.append('.').append(f)
        else sb.append('(').append(JString(f)).append(')')
      case Index(i) => sb.append('(').append(i).append(')')
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
