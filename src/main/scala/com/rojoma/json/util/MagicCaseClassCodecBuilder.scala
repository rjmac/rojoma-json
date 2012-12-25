package com.rojoma.json.util

import com.rojoma.`json-impl`.util.MagicCaseClassCodecBuilderImpl

import com.rojoma.json.codec.JsonCodec

object MagicCaseClassCodecBuilder {
  def apply[T]: JsonCodec[T] = macro MagicCaseClassCodecBuilderImpl.impl[T]
}
