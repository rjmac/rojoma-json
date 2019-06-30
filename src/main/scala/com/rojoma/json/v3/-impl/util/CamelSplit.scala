package com.rojoma.json.v3
package `-impl`.util

import scala.annotation.tailrec
import scala.collection.mutable

object CamelSplit {
  def apply(s: String) = breakUp(s, wordEnds(s))

  @tailrec
  private def breakUp(s: String, ends: List[Int], lastEnd: Int = 0, soFar: List[String] = Nil): List[String] = ends match {
    case n :: ns =>
      val l = s.substring(lastEnd, n)
      breakUp(s, ns, n, l :: soFar)
    case Nil =>
      (s.substring(lastEnd) :: soFar).reverse
  }

  private def wordEnds(s: String): List[Int] =
    findIndices(s.tails, breakp).map(1 + _).toList

  private def breakp(s: String): Boolean =
    (s.length >= 3 && s.charAt(0).isUpper && s.charAt(1).isUpper && s.charAt(2).isLower) ||
      (s.length >= 2 && s.charAt(0).isLower && s.charAt(1).isUpper)

  private def findIndices[T](it: IterableOnce[T], f: T => Boolean): List[Int] = {
    val lb = new mutable.ListBuffer[Int]
    var idx = 0
    it.iterator.foreach { place =>
      if(f(place)) lb += idx
      idx += 1
    }
    lb.toList
  }
}
