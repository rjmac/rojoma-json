package com.rojoma.json
package io

import java.io.{Writer, StringWriter}

private[io] object WriterUtils {
  def formatString(s: String): String = {
    val sw = new StringWriter
    writeString(s, sw)
    sw.toString
  }

  def writeString(s: String, output: Writer) {
    output.write('"')
    var i = 0
    val len = s.length
    while(i < len) {
      val c = s.charAt(i); i += 1

      c match {
        case '"' => output.write("\\\"")
        case '\\' => output.write("\\\\")
        case '\b' => output.write("\\b")
        case '\f' => output.write("\\f")
        case '\n' => output.write("\\n")
        case '\r' => output.write("\\r")
        case '\t' => output.write("\\t")
        case _ =>
          if(shouldEscape(c)) unicode(c, output)
          else output.write(c)
      }
    }
    output.write('"')
  }

  def shouldEscape(c: Char): Boolean = {
    val t = Character.getType(c)
    (t == Character.SURROGATE) || (t == Character.CONTROL) || (t == Character.UNASSIGNED) || (t == Character.PRIVATE_USE) || (t == Character.LINE_SEPARATOR) || (t == Character.PARAGRAPH_SEPARATOR)
  }

  def unicode(c: Char, output: Writer) = output.write("\\u%04x".format(c.toInt))
}

