package com.rojoma.json.v3.`-impl`.ast

import scala.{collection => sc}
import scala.runtime.AbstractFunction1

import com.rojoma.json.v3.ast.{JValue, JObject}

abstract class JObjectApply extends AbstractFunction1[sc.Map[String, JValue], JObject] { self: JObject.type =>
  override final def apply(fields: sc.Map[String, JValue]): JObject =
    if(fields.isEmpty) empty else new JObject(fields)
}
