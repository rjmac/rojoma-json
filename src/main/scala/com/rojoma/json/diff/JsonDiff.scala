package com.rojoma.json
package diff

import scala.{collection => sc}
import sc.{immutable => sci}

import ast._

class JsonDiffApplicationException extends Exception("Unable to apply diff; source object is not the right shape")
class JsonDiffMalformedException extends IllegalArgumentException("Malformed JSON diff")

sealed abstract class InnerJsonDiff {
  def write(writer: JsonDiff.JavaPrinter) = JsonDiff.write(writer, this, 0)
  def reverse: InnerJsonDiff
}

sealed abstract class JsonDiff extends InnerJsonDiff {
  def applyTo(value: JValue) = JsonDiff.applyTo(this, value)
  def reverse: JsonDiff
}
case class ObjectDiff(fieldDiffs: Map[String, InnerJsonDiff]) extends JsonDiff { self =>
  def reverse: ObjectDiff = new ObjectDiff(fieldDiffs.mapValues(_.reverse)) {
    override def reverse = self
  }
}

sealed abstract class ArrayDiff extends JsonDiff {
  def replacements: Map[Int, InnerJsonDiff]
  def reverse: ArrayDiff
}
case class ArrayDiffShortening(replacements: Map[Int, JsonDiff], removals: Seq[JValue]) extends ArrayDiff { self =>
  def reverse: ArrayDiffLengthening = new ArrayDiffLengthening(replacements.mapValues(_.reverse), removals) {
    override def reverse = self
  }
}
case class ArrayDiffLengthening(replacements: Map[Int, JsonDiff], additions: Seq[JValue]) extends ArrayDiff { self =>
  def reverse: ArrayDiffShortening = new ArrayDiffShortening(replacements.mapValues(_.reverse), additions)  {
    override def reverse = self
  }
}
case class ArrayDiffSameLength(replacements: Map[Int, JsonDiff]) extends ArrayDiff { self =>
  def reverse: ArrayDiffSameLength = new ArrayDiffSameLength(replacements.mapValues(_.reverse))  {
    override def reverse = self
  }
}
case class Replacement(oldValue: JValue, newValue: JValue) extends JsonDiff {
  def reverse = Replacement(newValue, oldValue)
}
case class Addition(newValue: JValue) extends InnerJsonDiff {
  def reverse = Removal(newValue)
}
case class Removal(oldValue: JValue) extends InnerJsonDiff {
  def reverse = Addition(oldValue)
}

object JsonDiff extends Function2[JValue, JValue, Option[JsonDiff]] {
  type JavaPrinter = { def println(thing: Any); def print(thing: Any) }
  private[diff] def write(writer: JavaPrinter, diff: InnerJsonDiff, indent: Int = 0) {
    def p(x: Any) {
      writer.print(" " * indent)
      writer.println(x)
    }

    def subDiff[K](k: K, v: InnerJsonDiff) {
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

    def arrayDiff(elemDiffs: Map[Int, JsonDiff]) {
      for((k,v) <- elemDiffs.toSeq.sorted(Ordering.by((x: (Int, JsonDiff)) => x._1)))
        subDiff(k,v)
    }

    diff match {
      case Addition(jvalue) =>
        p("+ " + jvalue)
      case Removal(jvalue) =>
        p("- " + jvalue)
      case Replacement(oldv, newv) =>
        p("- " + oldv)
        p("+ " + newv)
      case ArrayDiffSameLength(elemDiffs) =>
        arrayDiff(elemDiffs)
      case ArrayDiffLengthening(elemDiffs, additions) =>
        arrayDiff(elemDiffs)
        for(v <- additions)
          p("+ * : " + v)
      case ArrayDiffShortening(elemDiffs, removals) =>
        arrayDiff(elemDiffs)
        for(v <- removals)
          p("- * : " + v)
      case ObjectDiff(fieldDiffs) =>
        for((k, v) <- fieldDiffs.toSeq.sorted(Ordering.by((x: (String, InnerJsonDiff)) => x._1))) {
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

  private def objectDiff(aFields: sc.Map[String, JValue], bFields: sc.Map[String, JValue]): Option[ObjectDiff] = {
    val subsAndReplacements = aFields.foldLeft(Map.empty[String, InnerJsonDiff]) { (acc, aField) =>
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

  private def arrayDiff(aElements: Seq[JValue], bElements: Seq[JValue]): Option[ArrayDiff] = {
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

    if(aIt.hasNext) {
      Some(ArrayDiffShortening(prefixDiff, aIt.toSeq))
    } else if(bIt.hasNext) {
      Some(ArrayDiffLengthening(prefixDiff, bIt.toSeq))
    } else if(prefixDiff.nonEmpty) {
      Some(ArrayDiffSameLength(prefixDiff))
    } else {
      None
    }
  }

  private def failDiff(): Nothing = throw new JsonDiffApplicationException()
  private def failDiffMalformed(): Nothing = throw new JsonDiffMalformedException()

  private def processArray(value: JValue, changes: Map[Int, JsonDiff]): Array[JValue] = {
    val arr = value.cast[JArray].getOrElse(failDiff()).toSeq.toArray
    for((k, v) <- changes) {
      if(k < 0) failDiffMalformed()
      if(k >= arr.length) failDiff()
      arr(k) = applyTo(v, arr(k))
    }
    arr
  }

  private def applyTo(diff: JsonDiff, value: JValue): JValue = diff match {
    case ObjectDiff(fields) =>
      val newFields = fields.foldLeft(value.cast[JObject].getOrElse(failDiff()).toMap) { (objAcc, kv) =>
        val (k,v) = kv
        v match {
          case subdiff: JsonDiff =>
            objAcc.get(k) match {
              case Some(oldValue) =>
                objAcc + (k -> applyTo(subdiff, oldValue))
              case None =>
                failDiff()
            }
          case Addition(a) =>
            if(objAcc.contains(k)) failDiff()
            objAcc + (k -> a)
          case Removal(r) =>
            if(objAcc.get(k) != Some(r)) failDiff()
            objAcc - k
        }
      }
      JObject(newFields)
    case ArrayDiffSameLength(changes) =>
      JArray(processArray(value, changes))
    case ArrayDiffLengthening(changes, add) =>
      JArray(processArray(value, changes) ++ add)
    case ArrayDiffShortening(changes, sub) =>
      val shortenedArray = value match {
        case JArray(arr) =>
          if(arr.endsWith(sub)) {
            JArray(arr.dropRight(sub.length))
          } else failDiff()
        case _ =>
          failDiff()
      }
      JArray(processArray(shortenedArray, changes))
    case Replacement(oldValue, newValue) =>
      if(oldValue != value) failDiff()
      newValue
  }
}
