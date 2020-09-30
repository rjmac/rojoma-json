package com.rojoma.json.v3.`-impl`.ast

import scala.{collection => sc}
import scala.runtime.AbstractFunction1

import com.rojoma.json.v3.ast.{JValue, JArray}

abstract class JArrayApply extends AbstractFunction1[sc.Seq[JValue], JArray] { self: JArray.type =>
  override final def apply(elems: sc.Seq[JValue]): JArray =
    if(elems.isEmpty) empty else new JArray(elems)
}
