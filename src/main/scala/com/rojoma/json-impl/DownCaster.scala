package com.rojoma.`json-impl`

class DownCaster[T <: AnyRef](x : T) {
  def cast[U <: T](implicit cm: CM[U]): Option[U] = {
    val cls = erasureOf(cm)
    if(cls.isInstance(x)) Some(cls.cast(x).asInstanceOf[U])
    else None
  }
}
