package com.rojoma.json.v3
package util

import scala.language.experimental.macros

import `-impl`.util.AutomaticJsonCodecBuilderImpl
import codec._

object AutomaticJsonCodecBuilder {
  def apply[T]: JsonEncode[T] with JsonDecode[T] = macro AutomaticJsonCodecBuilderImpl.codec[T]
}

object AutomaticJsonEncodeBuilder {
  def apply[T]: JsonEncode[T] = macro AutomaticJsonCodecBuilderImpl.encode[T]
}

object AutomaticJsonDecodeBuilder {
  def apply[T]: JsonDecode[T] = macro AutomaticJsonCodecBuilderImpl.decode[T]
}
