package com.rojoma.json
package diff

import scala.{collection => sc}
import sc.{immutable => sci}

import ast._

object JsonDiff {
  type Path = Vector[PathElement]

  type Diff = Map[Path, DiffOp]

  // 2.9 has an ordering for seqs, but 2.8 doesn't, so...
  private implicit val pathOrdering = new Ordering[Path] {
    def compare(a: Path, b: Path): Int = {
      val aIt = a.iterator
      val bIt = b.iterator
 
      while(aIt.hasNext && bIt.hasNext) {
        val elemCmp = PathElementOrdering.compare(aIt.next(), bIt.next())
        if(elemCmp != 0) return elemCmp
      }

      if(aIt.hasNext) 1
      else if(bIt.hasNext) -1
      else 0
    }
  }

  val emptyDiff: Diff = sci.TreeMap.empty[Path, DiffOp]

  def jsonDiff(a: JValue, b: JValue): Diff = jsonDiff(a, b, Vector.empty, emptyDiff)

  private def jsonDiff(a: JValue, b: JValue, pathToHere: Path, accumulator: Diff): Diff = (a, b) match {
    case (JObject(aFields), JObject(bFields)) =>
      objectDiff(aFields, bFields, pathToHere, accumulator)
    case (JArray(aElements), JArray(bElements)) =>
      arrayDiff(aElements, bElements, pathToHere, accumulator)
    case (aValue, bValue) =>
      if (aValue == bValue) accumulator
      else accumulator + (pathToHere -> Replacement(aValue, bValue))
  }

  private def objectDiff(aFields: sc.Map[String, JValue], bFields: sc.Map[String, JValue], pathToHere: Path, accumulator: Diff): Diff = {
    val subsAndReplacements = aFields.foldLeft(accumulator) { (acc, aField) =>
      val (aKey, aValue) = aField
      val path = pathToHere :+ Field(aKey)
      bFields.get(aKey) match {
        case Some(bValue) =>
          jsonDiff(aValue, bValue, path, acc)
        case None =>
          acc + (path -> Removal(aValue))
      }
    }
    bFields.foldLeft(subsAndReplacements) { (diff, bKeyValue) =>
      val (bKey, bValue) = bKeyValue
      if(aFields contains bKey) // Already handled (it was a replacement or a noop)
        diff
      else // in B but not A -- it was added.
        diff + ((pathToHere :+ Field(bKey)) -> Addition(bValue))
    }
  }

  private def arrayDiff(aElements: Seq[JValue], bElements: Seq[JValue], pathToHere: Path, accumulator: Diff): Diff = {
    var aIt = aElements.iterator
    var bIt = bElements.iterator
    var idxIt = Iterator.from(0)

    var prefixDiff = accumulator
    while(aIt.hasNext && bIt.hasNext) {
      prefixDiff = jsonDiff(aIt.next(), bIt.next(), pathToHere :+ Index(idxIt.next()), prefixDiff)
    }

    if(aIt.hasNext) { // things were removed from the end
      aIt.zip(idxIt).foldLeft(prefixDiff) { (diff, aElemIdx) =>
        val (aElem, idx) = aElemIdx
        diff + ((pathToHere :+ Index(idx)) -> Removal(aElem))
      }
    } else if(bIt.hasNext) { // things were added on the end
      bIt.zip(idxIt).foldLeft(prefixDiff) { (diff, bElemIdx) =>
        val (bElem, idx) = bElemIdx
        diff + ((pathToHere :+ Index(idx)) -> Addition(bElem))
      }
    } else {
      prefixDiff
    }
  }
}
