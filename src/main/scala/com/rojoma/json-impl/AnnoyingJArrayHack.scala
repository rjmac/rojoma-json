package com.rojoma.`json-impl`

import com.rojoma.json.ast._

/** Workaround for bugs in 2.8's Seqs methods */
object AnnoyingJArrayHack {
  val isConvertForForceNecessaryView = // SI-4190
    try {
      List(1).view.map(_ + 1)(collection.breakOut) : IndexedSeq[Int]
      false
    } catch {
      case _: ClassCastException => true
    }
}
