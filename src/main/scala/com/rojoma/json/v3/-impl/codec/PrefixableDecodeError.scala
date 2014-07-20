package com.rojoma.json.v3
package `-impl`.codec

import codec._

class PrefixableDecodeError(val `private once 2.10 is gone`: DecodeError) extends AnyVal {
  def prefix(field: String) = `private once 2.10 is gone`.augment(Path.Field(field))
  def prefix(index: Int) = `private once 2.10 is gone`.augment(Path.Index(index))
}
