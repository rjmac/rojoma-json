package com.rojoma.json.v3
package `-impl`.ast

import ast._

object AnnoyingJArrayHack {
  @volatile
  private var breakOptimizer: IndexedSeq[Int] = null

  val isConvertForForceNecessaryView = // SI-4190
    try {
      breakOptimizer = List(1).view.map(_ + 1)(collection.breakOut)
      false
    } catch {
      case _: ClassCastException => true
    }

  def convertForForce(in: Seq[JValue]): Seq[JValue] =
    if(isConvertForForceNecessaryView && in.isInstanceOf[scala.collection.SeqViewLike[_, _, _]]) Vector(in : _*)
    else in

  def breakOpt = breakOptimizer // just to silence the unused warning
}
