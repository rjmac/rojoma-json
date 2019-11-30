package com.rojoma.json.v3
package `-impl`.interpolation

import com.rojoma.json.v3.codec.{JsonEncode, FieldEncode}
import com.rojoma.json.v3.ast.JValue

object Convert {
  def map[K, V](xs: Iterable[(K, V)])(implicit kEncode: FieldEncode[K], vEncode: JsonEncode[V]): Iterator[(String, JValue)] =
    xs.iterator.map { case (k, v) => (kEncode.encode(k), vEncode.encode(v)) }

  def array[T](xs: Iterable[T])(implicit encode: JsonEncode[T]): Iterator[JValue] =
    xs.iterator.map(encode.encode)

  def option[T](x: Option[T])(implicit encode: JsonEncode[T]): Option[JValue] =
    x.map(encode.encode)
}

// These two things are depreacted, but shouldn't be used in user
// code.  Keeping them around because they used to be emittred by the
// interpolator and so can live in library code.

object ConvertArrayItems {
  def apply[T](xs: Iterable[T])(implicit encode: codec.JsonEncode[T]) =
    xs.iterator.map(encode.encode)
}

object ConvertMapItems {
  def apply[K, V](xs: Iterable[(K, V)])(implicit kEncode: codec.FieldEncode[K], vEncode: codec.JsonEncode[V]) =
    xs.iterator.map { case (k, v) => kEncode.encode(k) -> vEncode.encode(v) }
}
