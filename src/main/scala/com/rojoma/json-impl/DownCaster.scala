package com.rojoma.`json-impl`

import scala.reflect.ClassTag

class DownCaster[T <: AnyRef](val `underlying-pseudoprivate` : T) extends AnyVal {
  def cast[U <: T](implicit cm: ClassTag[U]): Option[U] = {
    val cls = cm.runtimeClass
    if(cls.isInstance(`underlying-pseudoprivate`)) Some(cls.cast(`underlying-pseudoprivate`).asInstanceOf[U])
    else None
  }
}
