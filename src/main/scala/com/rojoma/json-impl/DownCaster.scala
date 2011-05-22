package com.rojoma.`json-impl`

class DownCaster[T <: AnyRef](x : T) {
  def cast[U <: T : ClassManifest]: Option[U] = {
    val cls = implicitly[ClassManifest[U]].erasure
    if(cls.isInstance(x)) Some(cls.cast(x).asInstanceOf[U])
    else None
  }
}
