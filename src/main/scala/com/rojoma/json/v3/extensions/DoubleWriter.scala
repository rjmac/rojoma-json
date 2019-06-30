package com.rojoma.json.v3
package extensions

import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import java.io.Writer

trait DoubleWriter {
  def toString(d: Double): String
  def toWriter(w: Writer, d: Double): Unit
}

private[extensions] class SimpleDoubleWriter extends DoubleWriter {
  def toString(d: Double) = d.toString
  def toWriter(w: Writer, d: Double) = w.write(d.toString)
}

object DoubleWriter {
  val doubleWriter: DoubleWriter = {
    val serviceLoader = ServiceLoader.load(classOf[DoubleWriter])
    serviceLoader.iterator().asScala.nextOption() match {
      case None =>
        Class.forName("com.rojoma.json.v3.extensions.SimpleDoubleWriter").newInstance().asInstanceOf[DoubleWriter]
      case Some(l) =>
        l
    }
  }
}
