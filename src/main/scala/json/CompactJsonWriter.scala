package json
package io

import scala.{collection=>sc}

import java.io.{Writer, StringWriter}

import ast.JValue

class CompactJsonWriter(output: Writer) extends JsonWriter {
  protected def writeArray(elements: sc.Seq[JValue]) {
    output.write('[')
    var didOne = false
    for(element <- elements) {
      if(didOne) output.write(',')
      else didOne = true
      write(element)
    }
    output.write(']')
  }

  protected def writeObject(fields: sc.Map[String, JValue]) {
    output.write('{')
    var didOne = false
    for((k, v) <- fields) {
      if(didOne) output.write(',')
      else didOne = true
      writeString(k)
      output.write(':')
      write(v)
    }
    output.write('}')
  }

  protected def writeBoolean(b: Boolean) {
    output.write(if(b) "true" else "false")
  }

  protected def writeNull() {
    output.write("null")
  }

  protected def writeDouble(x: Double) {
    output.write(x.toString)
  }

  protected def writeLong(x: Long) {
    output.write(x.toString)
  }

  protected def writeString(s: String) {
    WriterUtils.writeString(s, output)
  }
}

object CompactJsonWriter {
  def toWriter(w: Writer, datum: JValue) = new CompactJsonWriter(w).write(datum)
  def toString(datum: JValue) = {
    val w = new StringWriter
    toWriter(w, datum)
    w.toString
  }
}
