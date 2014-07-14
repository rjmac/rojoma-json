package com.rojoma.json.v3
package util

import scala.language.experimental.macros

import `-impl`.util.AutomaticHierarchyCodecBuilderImpl
import codec._

object AutomaticHierarchyEncodeBuilder {
  def apply[Root <: AnyRef](tagType: TagType): JsonEncode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.encodeTagged[Root]
  def apply[Root <: AnyRef](tagType: NoTag): JsonEncode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.encodeTagless[Root]
}
