package com.rojoma.json
package diff

sealed abstract class PathElement
case class Field(name: String) extends PathElement
case class Index(index: Int) extends PathElement

object PathElementOrdering extends Ordering[PathElement] {
  def compare(a: PathElement, b: PathElement) = (a, b) match {
    case (Field(aName), Field(bName)) => aName.compareTo(bName)
    case (Index(aIdx), Index(bIdx)) => aIdx.compareTo(bIdx)
    case (Field(_), Index(_)) => -1
    case (Index(_), Field(_)) => 1
  }
}
