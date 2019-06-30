package com.rojoma.json.v3
package util

import `-impl`.util._

/** Creates a combined [[com.rojoma.json.v3.codec.JsonEncode]]
  * and [[com.rojoma.json.v3.codec.JsonDecode]] for a simple wrapper type.
  * The `wrap` function may throw `IllegalArgumentException`; this
  * is translated to a [[com.rojoma.json.v3.codec.DecodeError.InvalidValue]].
  */
object WrapperJsonCodec {
  def apply[U] = new WrapperJsonCodecImpl[U]
}

/** Creates a [[com.rojoma.json.v3.codec.JsonEncode]] for a simple wrapper type.
  */
object WrapperJsonEncode {
  def apply[U] = new WrapperJsonEncodeImpl[U]
}

/** Creates a [[com.rojoma.json.v3.codec.JsonDecode]] for a simple wrapper type.
  * The `wrap` function may throw `IllegalArgumentException`; this
  * is translated to a [[com.rojoma.json.v3.codec.DecodeError.InvalidValue]].
  */
object WrapperJsonDecode {
  def apply[U] = new WrapperJsonDecodeImpl[U]
}
