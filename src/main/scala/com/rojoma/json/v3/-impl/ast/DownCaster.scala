package com.rojoma.json.v3
package `-impl`.ast

import ast._

class DownCaster[T <: JValue](val `private once 2.10 is no more` : T) extends AnyVal {
  def cast[U <: T](implicit j: Json[U]): Option[U] = {
    if(j.jsonTypes(`private once 2.10 is no more`.jsonType)) Some(`private once 2.10 is no more`.asInstanceOf[U])
    else None
  }
}
