package json
package io

import scala.{collection => sc}

import java.io.{Writer, StringWriter}

import ast._

private[io] case class PrettyContext(output: Writer, leftMargin: List[String], indentSize: Int, availableSpace: Int) {
  def indented = shifted(indentSize)
  def shifted(n: Int) = copy(leftMargin = (" " * n) :: leftMargin, availableSpace = availableSpace - n)

  def printMargin() = for(s <- leftMargin) output.write(s)
}

class PrettyJsonWriter private (context: PrettyContext) extends JsonWriter {
  def this(output: Writer, indentation: Int = 2, leftMargin: Int = 0, targetWidth: Int = 78) =
    this(PrettyContext(output, List(" " * leftMargin), indentation, targetWidth - leftMargin))

  // A "pretty writer" has two modes, compact and indented.
  // 
  // Lists format either "compactly" if they either have 0 or 1 atomic elements
  // or fit in the remaining target space:
  //   [ elem1, elem2, ... ]
  // or indented if they do not:
  //   [
  //     elem1,
  //     ...
  //   ]
  //
  // Objects format compactly if they fit in the available space or indented if they do not.
  // In the latter case, a field is formatted like
  //    "key" : "value"
  // if it fits in the remaining space, or
  //    "key" :
  //      "value"
  // if it does not.
  //
  // An empty list or object contains no internal space: [] or {}

  private def output = context.output

  private def isAtom(x: JValue) = x.isInstanceOf[JAtom]

  private def size(atom: JAtom) = atom match {
    case JNull => "null".length
    case JBoolean(x) => x.toString.length
    case JString(s) => WriterUtils.formatString(s).length
    case JIntegral(x) => x.toString.length
    case JFloatingPoint(x) => x.toString.length
  }

  private def willFitIn(elem: JValue, space: Int): Option[Int] = elem match {
    case atom: JAtom => Some(size(atom))
    case JArray(elems) =>
      elems match {
        case sc.Seq() =>
          Some("[]".length)
        case sc.Seq(atom: JAtom) =>
          Some("[  ]".length + size(atom))
        case _ =>
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

  private def writeArrayCompactly(elements: Seq[JValue]) {
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

  protected def writeDouble(x: Double) {
    if(x.isNaN || x.isInfinite) throw JsonInvalidFloat(x)
    output.write(x.toString)
  }

  protected def writeLong(x: Long) {
    output.write(x.toString)
  }

  protected def writeString(s: String) {
    WriterUtils.writeString(s, output)
  }
}

object PrettyJsonWriter {
  def toWriter(w: Writer, datum: JValue) = new PrettyJsonWriter(w).write(datum)
  def toString(datum: JValue) = {
    val w = new StringWriter
    toWriter(w, datum)
    w.toString
  }
}
