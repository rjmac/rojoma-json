package com.rojoma.json.util

import com.rojoma.`json-impl`.util.AutomaticJsonCodecBuilderImpl

import com.rojoma.json.codec.JsonCodec

object AutomaticJsonCodecBuilder {
  def apply[T]: JsonCodec[T] = macro AutomaticJsonCodecBuilderImpl.impl[T]
}