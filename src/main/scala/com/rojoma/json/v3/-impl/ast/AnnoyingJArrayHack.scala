package com.rojoma.json.v3
package `-impl`.ast

object AnnoyingJArrayHack {
  val isConvertForForceNecessaryView = // SI-4190
    try {
      List(1).view.map(_ + 1)(collection.breakOut) : IndexedSeq[Int]
      false
    } catch {
      case _: ClassCastException => true
    }
}
