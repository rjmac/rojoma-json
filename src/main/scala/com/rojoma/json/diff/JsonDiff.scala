package com.rojoma.json
package diff

import scala.{collection => sc}
import sc.{immutable => sci}

import ast._

sealed abstract class JsonDiff {
  def write(writer: JsonDiff.JavaPrinter) = JsonDiff.write(writer, this, 0)
  def reverse: JsonDiff
}
case class ObjectDiff(fieldDiffs: Map[String, JsonDiff]) extends JsonDiff { self =>
  def reverse: ObjectDiff = new ObjectDiff(fieldDiffs.mapValues(_.reverse)) {
    override def reverse = self
  }
}
case class ArrayDiff(elementDiffs: Map[Int, JsonDiff]) extends JsonDiff { self =>
  def reverse: ArrayDiff = new ArrayDiff(elementDiffs.mapValues(_.reverse))  {
    override def reverse = self
  }
}
case class Addition(newValue: JValue) extends JsonDiff {
  def reverse = Removal(newValue)
}
case class Replacement(oldValue: JValue, newValue: JValue) extends JsonDiff {
  def reverse = Replacement(newValue, oldValue)
}
case class Removal(oldValue: JValue) extends JsonDiff {
  def reverse = Addition(oldValue)
}

object JsonDiff extends Function2[JValue, JValue, Option[JsonDiff]] {
  type JavaPrinter = { def println(thing: Any); def print(thing: Any) }
  private def write(writer: JavaPrinter, diff: JsonDiff, indent: Int = 0) {
    def p(x: Any) {
      writer.print(" " * indent)
      writer.println(x)
    }

    def subDiff[K](k: K, v: JsonDiff) {
      v match {
        case Addition(jvalue) =>
          p("+ " + k + " : " + jvalue)
        case Removal(jvalue) =>
          p("- " + k + " : " + jvalue)
        case Replacement(oldv, newv) =>
          p("- " + k + " : " + oldv)
          p("+ " + k + " : " + newv)
        case _ =>
          p("! " + k + " :")
          write(writer, v, indent + 2)
      }
    }

    diff match {
      case Addition(jvalue) =>
        p("+ " + jvalue)
      case Removal(jvalue) =>
        p("- " + jvalue)
      case Replacement(oldv, newv) =>
        p("- " + oldv)
        p("+ " + newv)
      case ArrayDiff(elemDiffs) =>
        for((k, v) <- elemDiffs.toSeq.sorted(Ordering.by((x: (Int, JsonDiff)) => x._1))) {
          subDiff(k, v)
        }
      case ObjectDiff(fieldDiffs) =>
        for((k, v) <- fieldDiffs.toSeq.sorted(Ordering.by((x: (String, JsonDiff)) => x._1))) {
          subDiff(JString(k), v) // wrap k in a jstring so this output has some hope of being machine-parsable
        }
    }
  }

  def apply(a: JValue, b: JValue): Option[JsonDiff] = (a, b) match {
    case (JObject(aFields), JObject(bFields)) =>
      objectDiff(aFields, bFields)
    case (JArray(aElements), JArray(bElements)) =>
      arrayDiff(aElements, bElements)
    case (aValue, bValue) =>
      if (aValue == bValue) None
      else Some(Replacement(aValue, bValue))
  }

  private def objectDiff(aFields: sc.Map[String, JValue], bFields: sc.Map[String, JValue]): Option[JsonDiff] = {
    val subsAndReplacements = aFields.foldLeft(Map.empty[String, JsonDiff]) { (acc, aField) =>
      val (aKey, aValue) = aField
      bFields.get(aKey) match {
        case Some(bValue) =>
          apply(aValue, bValue) match {
            case Some(diff) =>
              acc + (aKey -> diff)
            case None =>
              acc
          }
        case None =>
          acc + (aKey -> Removal(aValue))
      }
    }
    val fullDiff = bFields.foldLeft(subsAndReplacements) { (diff, bKeyValue) =>
      val (bKey, bValue) = bKeyValue
      if(aFields contains bKey) // Already handled (it was a replacement or a noop)
        diff
      else // in B but not A -- it was added.
        diff + (bKey -> Addition(bValue))
    }
    if(fullDiff.isEmpty) None
    else Some(ObjectDiff(fullDiff))
  }

  private def arrayDiff(aElements: Seq[JValue], bElements: Seq[JValue]): Option[JsonDiff] = {
    var aIt = aElements.iterator
    var bIt = bElements.iterator
    var idxIt = Iterator.from(0)

    var prefixDiff = Map.empty[Int, JsonDiff]
    while(aIt.hasNext && bIt.hasNext) {
      val idx = idxIt.next()
      apply(aIt.next(), bIt.next()) match {
        case Some(subDiff) => prefixDiff += (idx -> subDiff)
        case None => /* noop */
      }
    }

    val fullDiff =
      if(aIt.hasNext) { // things were removed from the end
        aIt.zip(idxIt).foldLeft(prefixDiff) { (diff, aElemIdx) =>
          val (aElem, idx) = aElemIdx
          diff + (idx -> Removal(aElem))
        }
      } else if(bIt.hasNext) { // things were added on the end
        bIt.zip(idxIt).foldLeft(prefixDiff) { (diff, bElemIdx) =>
          val (bElem, idx) = bElemIdx
          diff + (idx -> Addition(bElem))
        }
      } else {
        prefixDiff
      }

    if(fullDiff.isEmpty) None
    else Some(ArrayDiff(fullDiff))
  }
}
