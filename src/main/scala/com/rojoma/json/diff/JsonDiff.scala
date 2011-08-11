package com.rojoma.json
package diff

import scala.{collection => sc}
import sc.{immutable => sci}

import ast._

object JsonDiff {
  def printDiff(diff: DiffTree, indent: Int = 0) {
    def p(x: Any) {
      print(" " * indent)
      println(x)
    }

    def subDiff[K](k: K, v: DiffTree) {
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
          printDiff(v, indent + 2)
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
        for((k, v) <- elemDiffs.toSeq.sorted(Ordering.by((x: (Int, DiffTree)) => x._1))) {
          subDiff(k, v)
        }
      case ObjectDiff(fieldDiffs) =>
        for((k, v) <- fieldDiffs.toSeq.sorted(Ordering.by((x: (String, DiffTree)) => x._1))) {
          subDiff(JString(k), v) // wrap k in a jstring so this output has some hope of being machine-parsable
        }
    }
  }

  def jsonDiff(a: JValue, b: JValue): Option[DiffTree] = (a, b) match {
    case (JObject(aFields), JObject(bFields)) =>
      objectDiff(aFields, bFields)
    case (JArray(aElements), JArray(bElements)) =>
      arrayDiff(aElements, bElements)
    case (aValue, bValue) =>
      if (aValue == bValue) None
      else Some(Replacement(aValue, bValue))
  }

  private def objectDiff(aFields: sc.Map[String, JValue], bFields: sc.Map[String, JValue]): Option[DiffTree] = {
    val subsAndReplacements = aFields.foldLeft(Map.empty[String, DiffTree]) { (acc, aField) =>
      val (aKey, aValue) = aField
      bFields.get(aKey) match {
        case Some(bValue) =>
          jsonDiff(aValue, bValue) match {
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

  private def arrayDiff(aElements: Seq[JValue], bElements: Seq[JValue]): Option[DiffTree] = {
    var aIt = aElements.iterator
    var bIt = bElements.iterator
    var idxIt = Iterator.from(0)

    var prefixDiff = Map.empty[Int, DiffTree]
    while(aIt.hasNext && bIt.hasNext) {
      val idx = idxIt.next()
      jsonDiff(aIt.next(), bIt.next()) match {
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
