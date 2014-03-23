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
    val fastEnd = gallop(s, 0)
    if(fastEnd == s.length) {
      output.write(s)
    } else {
      slowPath(s, fastEnd, output: Writer)
    }
    output.write('"')
  }

  private def slowPath(s: String, endOfFastPrefix: Int, output: Writer) {
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

  private def gallop(s: String, start: Int): Int = {
    var i = start
    val end = s.length
    while(i != end && fastCopy(s.charAt(i))) i += 1
    i
  }

  private def fastCopy(c: Char): Boolean = c >= ' ' && c <= '~' && c != '"' && c != '\\'

  def shouldEscape(c: Char): Boolean = {
    val t = Character.getType(c)
    (t == Character.SURROGATE) || (t == Character.CONTROL) || (t == Character.UNASSIGNED) || (t == Character.PRIVATE_USE) || (t == Character.LINE_SEPARATOR) || (t == Character.PARAGRAPH_SEPARATOR)
  }

  def unicode(c: Char, output: Writer) = output.write("\\u%04x".format(c.toInt))
}

