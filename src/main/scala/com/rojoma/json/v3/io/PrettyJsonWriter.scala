package com.rojoma.json.v3
package io

import scala.{collection => sc}
import java.io.{Writer, StringWriter}

import ast._

private[io] case class PrettyContext(output: Writer, leftMargin: List[String], indentSize: Int, availableSpace: Int) {
  def indented = shifted(indentSize)
  def shifted(n: Int) = copy(leftMargin = (" " * n) :: leftMargin, availableSpace = availableSpace - n)

  def printMargin() = for(s <- leftMargin) output.write(s)
}

/** An object that will write [[com.rojoma.json.v3.ast.JValue]]s in a human-friendly
  * indented format.
  *
  * The writer will try to keep as much data on a single line as
  * possible.  As a result, arrays and objects have two printing
  * modes (selected automatically), "compact" and "indented".
  *
  * In the "compact" mode, an array is stored on a single line:
  * {{{
  *    [ elem1, elem2, ... ]
  * }}}
  * Similarly for objects:
  * {{{
  *    { "key1" : value1, "key2 : value2, ... }
  * }}}
  *
  * In the "indented" mode, the delimiters appear on lines by
  * themselves, with each element formatted on a separate line:
  * {{{
  *    [
  *      elem1,
  *      elem2,
  *      ...
  *    ]
  * }}}
  * For objects, if a key/value pair does not fit on a single line,
  * the value is printed on a line of its own, indented further:
  * {{{
  *    {
  *      "key" :
  *        value
  *    }
  * }}}
  * 
  * Empty arrays and objects are always represented compactly,
  * as `[]` or `{}` respectively.
  *
  * This does many small writes, so it is probably a good idea to wrap
  * the `Writer` in a `BufferedWriter`. */
class PrettyJsonWriter private (context: PrettyContext) extends JsonWriter {
  def this(output: Writer, indentation: Int = 2, leftMargin: Int = 0, targetWidth: Int = 78) =
    this(PrettyContext(output, List(" " * leftMargin), indentation, targetWidth))
  private def output = context.output

  private def isAtom(x: JValue) = x.isInstanceOf[JAtom]

  private def size(atom: JAtom) = atom match {
    case JNull => "null".length
    case JBoolean(x) => x.toString.length
    case JString(s) => WriterUtils.formatString(s).length
    case n: JNumber => n.toString.length
  }

  private def willFitIn(elem: JValue, space: Int): Option[Int] = elem match {
    case atom: JAtom => Some(size(atom))
    case JArray(elems) =>
      if(elems.isEmpty) {
        Some("[]".length)
      } else {
        var remaining = space - "[  ]".length
        val it = elems.iterator
        var didOne = false
        while(it.hasNext) {
          if(didOne) remaining -= ", ".length
          else didOne = true
          willFitIn(it.next(), remaining) match {
            case None => return None
            case Some(size) => remaining -= size
          }
        }
        if(remaining >= 0) Some(space - remaining)
        else None
      }
    case JObject(fields) =>
      if(fields.isEmpty) {
        Some("{}".length)
      } else {
        var remaining = space - "{  }".length
        val it = fields.iterator
        var didOne = false
        while(it.hasNext) {
          if(didOne) remaining -= ", ".length
          else didOne = true

          remaining -= " : ".length

          val (k, v) = it.next()
          
          willFitIn(JString(k), remaining) match {
            case None => return None
            case Some(size) => remaining -= size
          }
          willFitIn(v, remaining) match {
            case None => return None
            case Some(size) => remaining -= size
          }
        }
        if(remaining >= 0) Some(space - remaining)
        else None
      }
  }

  private def writeCompactly(jobject: JValue) {
    jobject match {
      case JArray(elements) =>
        writeArrayCompactly(elements)
      case JObject(fields) =>
        writeObjectCompactly(fields)
      case other: JAtom =>
        write(other)
    }
  }

  protected def writeArray(elements: sc.Seq[JValue]) {
    if(willFitIn(JArray(elements), context.availableSpace).isDefined) {
      writeArrayCompactly(elements)
    } else {
      output.write("[\n")
      val newContext = context.indented
      val newWriter = new PrettyJsonWriter(newContext)
      newContext.printMargin()
      var didOne = false
      for(elem <- elements) {
        if(didOne) { output.write(",\n"); newContext.printMargin() }
        else didOne = true
        newWriter.write(elem)
      }
      output.write("\n")
      context.printMargin()
      output.write("]")
    }
  }

  protected def writeObject(fields: sc.Map[String, JValue]) {
    if(willFitIn(JObject(fields), context.availableSpace).isDefined) {
      writeObjectCompactly(fields)
    } else {
      output.write("{\n")
      val newContext = context.indented
      newContext.printMargin()
      var didOne = false
      for((k, v) <- fields) {
        if(didOne) { output.write(",\n"); newContext.printMargin() }
        else didOne = true

        val key = WriterUtils.formatString(k)
        output.write(key)
        val spaceForValue = context.availableSpace - key.length - " : ".length
        if(willFitIn(v, spaceForValue).isDefined) {
          output.write(" : ")
          val newerContext = newContext.shifted(key.length + " : ".length)
          val newerWriter = new PrettyJsonWriter(newerContext)
          newerWriter.write(v)
        } else {
          output.write(" :\n")
          val newerContext = newContext.indented
          val newerWriter = new PrettyJsonWriter(newerContext)
          newerContext.printMargin()
          newerWriter.write(v)
        }
      }
      output.write("\n")
      context.printMargin()
      output.write("}")
    }
  }

  private def writeArrayCompactly(elements: sc.Seq[JValue]) {
    if(elements.isEmpty) {
      output.write("[]")
    } else {
      output.write("[ ")
      var didOne = false
      for(element <- elements) {
        if(didOne) output.write(", ")
        else didOne = true
        writeCompactly(element)
      }
      output.write(" ]")
    }
  }

  private def writeObjectCompactly(fields: sc.Map[String, JValue]) {
    if(fields.isEmpty) {
      output.write("{}")
    } else {
      output.write("{ ")
      var didOne = false
      for((k, v) <- fields) {
        if(didOne) output.write(", ")
        else didOne = true
        WriterUtils.writeString(k, output)
        output.write(" : ")
        writeCompactly(v)
      }
      output.write(" }")
    }
  }

  protected def writeBoolean(b: Boolean) {
    output.write(if(b) "true" else "false")
  }

  protected def writeNull() {
    output.write("null")
  }

  protected def writeNumber(x: JNumber) {
    output.write(x.toString)
  }

  protected def writeString(s: String) {
    WriterUtils.writeString(s, output)
  }
}

object PrettyJsonWriter {
  /** Utility function for writing a single datum to a `Writer`.
    * @throws `IOException` if a low-level IO exception occurs.
    * @see [[com.rojoma.json.v3.io.PrettyJsonWriter]] */
  @throws(classOf[java.io.IOException])
  def toWriter(w: Writer, datum: JValue) = new PrettyJsonWriter(w).write(datum)

  /** Utility function for writing a single datum to a `String`.
    * @return The encoded JSON object.
    * @see [[com.rojoma.json.v3.io.PrettyJsonWriter]] */
  def toString(datum: JValue) = {
    val w = new StringWriter
    toWriter(w, datum)
    w.toString
  }
}
