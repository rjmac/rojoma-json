package com.rojoma.json.v3
package extensions

import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import java.io.Writer

trait StringWriter {
  def toString(s: String): String
  def toWriter(w: Writer, s: String): Unit
}

private[extensions] class SimpleStringWriter extends StringWriter {
  def toString(s: String) = {
    val sw = new java.io.StringWriter
    toWriter(sw, s)
    sw.toString
  }

  def toWriter(output: Writer, s: String): Unit = {
    output.write('"')
    val fastEnd = gallop(s, 0)
    if(fastEnd == s.length) {
      output.write(s)
    } else {
      slowPath(s, fastEnd, output: Writer)
    }
    output.write('"')
  }

  def slowPath(s: String, endOfFastPrefix: Int, output: Writer): Unit = {
    if(endOfFastPrefix != 0) output.write(s, 0, endOfFastPrefix)
    var i = endOfFastPrefix
    val len = s.length
    do {
      val c = s.charAt(i)

      c match {
        case '"' => output.write("\\\"")
        case '\\' => output.write("\\\\")
        case '\b' => output.write("\\b")
        case '\f' => output.write("\\f")
        case '\n' => output.write("\\n")
        case '\r' => output.write("\\r")
        case '\t' => output.write("\\t")
        case _ if shouldEscape(c) => unicode(c, output)
        case _ if fastCopy(c) =>
          val end = gallop(s, i + 1)
          output.write(s, i, end - i)
          i = end - 1
        case _ =>
          output.write(c)
      }

      i += 1
    } while(i != len)
  }

  def gallop(s: String, start: Int): Int = {
    var i = start
    val end = s.length
    while(i != end && fastCopy(s.charAt(i))) i += 1
    i
  }

  def fastCopy(c: Char): Boolean = c >= ' ' && c <= '~' && c != '"' && c != '\\'

  def shouldEscape(c: Char): Boolean = {
    val t = Character.getType(c)
    (t == Character.SURROGATE) || (t == Character.CONTROL) || (t == Character.UNASSIGNED) || (t == Character.PRIVATE_USE) || (t == Character.LINE_SEPARATOR) || (t == Character.PARAGRAPH_SEPARATOR)
  }

  def unicode(c: Char, output: Writer) = output.write("\\u%04x".format(c.toInt))
}

object StringWriter {
  val stringWriter: StringWriter = {
    val serviceLoader = ServiceLoader.load(classOf[StringWriter])
    serviceLoader.iterator().asScala.nextOption() match {
      case None =>
        Class.forName("com.rojoma.json.v3.extensions.SimpleStringWriter").newInstance().asInstanceOf[StringWriter]
      case Some(l) =>
        l
    }
  }
}
