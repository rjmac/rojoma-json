package com.rojoma.json.v3
package `-impl`.interpolation

object ConvertMapItems {
  def apply[K, V](xs: Iterable[(K, V)])(implicit kEncode: codec.FieldEncode[K], vEncode: codec.JsonEncode[V]) =
    xs.iterator.map { case (k, v) => kEncode.encode(k) -> vEncode.encode(v) }
}
