package com.rojoma.json.v3.`-impl`

import scala.reflect.ClassTag

class GenericDownCaster[T <: AnyRef](x : T) {
  def cast[U <: T : ClassTag]: Option[U] = {
    val cls = implicitly[ClassTag[U]].runtimeClass
    if(cls.isInstance(x)) Some(cls.cast(x).asInstanceOf[U])
    else None
  }
}
