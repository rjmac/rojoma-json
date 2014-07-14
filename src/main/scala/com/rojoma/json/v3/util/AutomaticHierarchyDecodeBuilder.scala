package com.rojoma.json.v3
package util

import scala.language.experimental.macros

import `-impl`.util.AutomaticHierarchyCodecBuilderImpl
import codec._

object AutomaticHierarchyDecodeBuilder {
  def apply[Root <: AnyRef](tagType: TagType): JsonDecode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.decodeTagged[Root]
  def apply[Root <: AnyRef](tagType: NoTag): JsonDecode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.decodeTagless[Root]
}
