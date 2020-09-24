package com.rojoma.json.v3
package extensions

import scala.collection.JavaConverters._
import java.lang.reflect.InvocationTargetException
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
    serviceLoader.iterator().asScala.toStream.headOption match {
      case None =>
        try {
          Class.forName("com.rojoma.json.v3.extensions.SimpleDoubleWriter").getConstructor().newInstance().asInstanceOf[DoubleWriter]
        } catch {
          case e: InvocationTargetException if e.getCause != null =>
            throw e.getCause
        }
      case Some(l) =>
        l
    }
  }
}
