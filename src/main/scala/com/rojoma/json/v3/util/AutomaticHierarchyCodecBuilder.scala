package com.rojoma.json.v3
package util

import scala.language.experimental.macros

import `-impl`.util.AutomaticHierarchyCodecBuilderImpl
import codec._

object AutomaticHierarchyCodecBuilder {
  def apply[Root <: AnyRef](tagType: TagType): JsonEncode[Root] with JsonDecode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.codecTagged[Root]
  def apply[Root <: AnyRef](tagType: NoTag): JsonEncode[Root] with JsonDecode[Root] =
    macro AutomaticHierarchyCodecBuilderImpl.codecTagless[Root]
}
