package com.rojoma.json.v3
package `-impl`.interpolation

import com.rojoma.json.v3.codec.{JsonEncode, FieldEncode}
import com.rojoma.json.v3.ast.JValue

object Convert {
  def map[K, V](xs: Iterable[(K, V)])(using kEncode: FieldEncode[K], vEncode: JsonEncode[V]): Iterator[(String, JValue)] =
    xs.iterator.map { case (k, v) => (kEncode.encode(k), vEncode.encode(v)) }

  def array[T](xs: Iterable[T])(using encode: JsonEncode[T]): Iterator[JValue] =
    xs.iterator.map(encode.encode)

  def option[T](x: Option[T])(using encode: JsonEncode[T]): Option[JValue] =
    x.map(encode.encode)
}
