package com.rojoma.`json-impl`

import com.rojoma.json.ast._

/** Workaround for bugs in 2.8's Seqs methods */
object AnnoyingJArrayHack {
  val isConvertForEqualsNecessary = Nil != Array[AnyRef]().toSeq

  def convertForEquals(in: Seq[JValue]): Seq[JValue] =
    if(isConvertForEqualsNecessary && in.isInstanceOf[scala.collection.mutable.WrappedArray[_]]) Vector(in: _*)
    else in

  val isConvertForForceNecessaryStream =
    try {
      Stream(1).map(_ + 1)(collection.breakOut) : IndexedSeq[Int]
      false
    } catch {
      case _: ClassCastException => true
    }

  val isConvertForForceNecessaryView = // SI-4190
    try {
      List(1).view.map(_ + 1)(collection.breakOut) : IndexedSeq[Int]
      false
    } catch {
      case _: ClassCastException => true
    }

  def convertForForce(in: Seq[JValue]): Seq[JValue] =
    if(isConvertForForceNecessaryStream && in.isInstanceOf[Stream[_]]) Vector(in : _*)
    else if(isConvertForForceNecessaryView && in.isInstanceOf[scala.collection.SeqViewLike[_, _, _]]) Vector(in : _*)
    else in
}
