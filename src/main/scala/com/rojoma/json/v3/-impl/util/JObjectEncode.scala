package com.rojoma.json.v3.`-impl`.util

import com.rojoma.json.v3.ast.JObject
import com.rojoma.json.v3.codec.JsonEncode

trait JObjectEncode[T] { self: JsonEncode[T] =>
  override def encode(x: T): JObject
}
