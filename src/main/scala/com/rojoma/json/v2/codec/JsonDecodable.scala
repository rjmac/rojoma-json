package com.rojoma
package json.v2
package codec

import ast._

trait JsonDecodable[+T] {
  def decode(x: JValue): JsonValidation[T]
}
