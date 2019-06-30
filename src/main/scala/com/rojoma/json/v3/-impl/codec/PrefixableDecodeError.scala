package com.rojoma.json.v3
package `-impl`.codec

import codec._

class PrefixableDecodeError(private val err: DecodeError) extends AnyVal {
  def prefix(field: String) = err.augment(Path.Field(field))
  def prefix(index: Int) = err.augment(Path.Index(index))
}
