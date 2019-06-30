package com.rojoma.json.v3
package `-impl`.ast

import ast._

class DownCaster[T <: JValue](private val x: T) extends AnyVal {
  def cast[U <: T](implicit j: Json[U]): Option[U] = {
    if(j.jsonTypes(x.jsonType)) Some(x.asInstanceOf[U])
    else None
  }
}
