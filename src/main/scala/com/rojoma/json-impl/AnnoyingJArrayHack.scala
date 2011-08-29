package com.rojoma.`json-impl`

import com.rojoma.json.ast._

/** Workaround for bugs in 2.8's Seqs methods */
object AnnoyingJArrayHack {
  val isConvertForEqualsNecessary = Nil != Array[AnyRef]().toSeq

  def convertForEquals(in: Seq[JValue]): Seq[JValue] =
    if(isConvertForEqualsNecessary && in.isInstanceOf[scala.collection.mutable.WrappedArray[_]]) Vector(in: _*)
    else in

  val isConvertForForceNecessary =
    try {
      Stream(1).map(_ + 1)(collection.breakOut) : IndexedSeq[Int]
      false
    } catch {
      case _: ClassCastException => true
    }

  def convertForForce(in: Seq[JValue]): Seq[JValue] =
    if(isConvertForForceNecessary && in.isInstanceOf[Stream[_]]) Vector(in : _*)
    else in
}
