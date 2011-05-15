package json
package ast

import scala.{collection => sc}

sealed abstract class JValue

object JValue {
  // This can be used for a pretty nice syntax:
  //   for {
  //     JObject(foo) <- raw.cast[JObject]
  //     JArray(elems) <- foo.get("foo").flatMap(_.cast[JArray])
  //   } yield {
  //     ...something with elems...
  //   } getOrElse(throw "couldn't find interesting elements")
  implicit def toCastable[T <: JValue](x: T) = new `ast-impl`.DownCaster(x)
}

sealed abstract class JAtom extends JValue

sealed abstract class JNumber extends JAtom {
  def floatingPoint: Double
  def integral: Long
}

object JNumber {
  def apply(x: Double): JNumber = JFloatingPoint(x)
  def apply(x: Long): JNumber = JIntegral(x)
}

case class JFloatingPoint(floatingPoint: Double) extends JNumber {
  def integral = floatingPoint.toLong
}

case class JIntegral(integral: Long) extends JNumber {
  def floatingPoint = integral.toDouble
}

case class JString(string: String) extends JAtom

case class JBoolean(boolean: Boolean) extends JAtom

case object JNull extends JAtom

sealed abstract class JCompound extends JValue

case class JArray(override val toSeq: sc.Seq[JValue]) extends JCompound with Iterable[JValue] with PartialFunction[Int, JValue] {
  override def size = toSeq.size
  def length = size
  override def toIndexedSeq[B >: JValue] = toSeq.toIndexedSeq[B]
  override def toList = toSeq.toList
  override def toStream = toSeq.toStream
  override def toArray[B >: JValue : ClassManifest] = toSeq.toArray[B]

  def apply(idx: Int) = toSeq(idx)
  def isDefinedAt(idx: Int) = toSeq.isDefinedAt(idx)
  def iterator = toSeq.iterator
}

case class JObject(val fields: sc.Map[String, JValue]) extends JCompound with Iterable[(String, JValue)] with PartialFunction[String, JValue] {
  override def size = fields.size
  def contains(s: String) = fields.contains(s)
  def apply(key: String) = fields(key)
  def get(key: String) = fields.get(key)
  def getOrElse[B1 >: JValue](key: String, default: => B1): B1 = fields.getOrElse(key, default)
  def isDefinedAt(key: String) = fields.isDefinedAt(key)
  def iterator = fields.iterator
  def keys = fields.keys
  def keysIterator = fields.keysIterator
  def keySet = fields.keySet
  def values = fields.values
  def valuesIterator = fields.valuesIterator
  def mapValues[C](f: JValue => C): sc.Map[String, C] = fields.mapValues(f)
  override def toSeq = fields.toSeq
  override def toMap[T, U] (implicit ev: <:<[(String, JValue), (T, U)]): Map[T, U] = fields.toMap
}
