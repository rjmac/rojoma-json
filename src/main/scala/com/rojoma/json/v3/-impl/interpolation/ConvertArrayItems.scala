package com.rojoma.json.v3
package `-impl`.interpolation

object ConvertArrayItems {
  def apply[T](xs: Iterable[T])(implicit encode: codec.JsonEncode[T]) =
    xs.iterator.map(encode.encode)
}
