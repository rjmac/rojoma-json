package com.rojoma.json.v3.`-impl`.ast

import scala.runtime.AbstractFunction1

import com.rojoma.json.v3.ast.JBoolean

abstract class JBooleanApply extends AbstractFunction1[Boolean, JBoolean] { self: JBoolean.type =>
  override final def apply(boolean: Boolean): JBoolean =
    if(boolean) canonicalTrue else canonicalFalse
}
