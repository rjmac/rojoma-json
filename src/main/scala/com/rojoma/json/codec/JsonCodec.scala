package com.rojoma.json
package codec

import ast._

trait JsonCodec[T] {
  def encode(x: T): JValue
  def decode(x: JValue): Option[T]
}
