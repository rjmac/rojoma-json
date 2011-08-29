package com.rojoma.`json-impl`

import com.rojoma.json.ast._

/** Workaround for bugs in 2.8's Seqs' equals() methods */
object AnnoyingJArrayHack {
  val isNecessary = Nil != Array[AnyRef]().toSeq

  def convert(in: Seq[JValue]): Seq[JValue] =
    if(isNecessary && in.isInstanceOf[scala.collection.mutable.WrappedArray[_]]) Vector(in: _*)
    else in
}
