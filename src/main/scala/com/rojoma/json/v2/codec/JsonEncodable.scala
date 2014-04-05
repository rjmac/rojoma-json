package com.rojoma
package json.v2
package codec

import ast._

trait JsonEncodable[-T] {
  def encode(x: T): JValue
}
