package com.rojoma.json.v3
package io

import scala.{collection=>sc}

import java.io.{Writer, StringWriter}

import ast._

/** An object that will write [[com.rojoma.json.v3.ast.JValue]]s in a non-human-friendly
  * "compact" format with no spaces or newlines.  This does many small
  * writes, so it is probably a good idea to wrap the `Writer` in a `BufferedWriter`. */
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

  protected def writeNumber(x: JNumber) {
    x.toWriter(output)
  }

  protected def writeString(s: String) {
    WriterUtils.writeString(s, output)
  }
}

object CompactJsonWriter {
  /** Utility function for writing a single datum to a `Writer`.  This
    * does many small writes, so it is probably a good idea to wrap
    * the `Writer` in a `BufferedWriter`.
    *
    * @throws `IOException` if a low-level IO exception occurs.
    * @see [[com.rojoma.json.v3.io.CompactJsonWriter]] */
  @throws(classOf[java.io.IOException])
  def toWriter(w: Writer, datum: JValue) = new CompactJsonWriter(w).write(datum)

  /** Utility function for writing a single datum to a `String`.
    *
    * @return The encoded JSON object.
    * @see [[com.rojoma.json.v3.io.CompactJsonWriter]] */
  def toString(datum: JValue) = {
    val w = new StringWriter
    toWriter(w, datum)
    w.toString
  }
}
