package com.rojoma.json
package zipper

import scala.annotation.tailrec

import ast._

sealed trait ZipperLike {
  def up: Option[JsonZipper]
  def up_! : JsonZipper

  def next: Option[JsonZipper] // these will be "none" if the parent is not an array
  def prev: Option[JsonZipper] // or the sibling if it is

  def next_! = next.getOrElse(throw new NoSuchElementException("No next sibling"))
  def prev_! = prev.getOrElse(throw new NoSuchElementException("No previous sibling"))

  def sibling(field: String): Option[JsonZipper] // this will be "none" if the parent is not an object
  def sibling_!(field: String) = sibling(field).getOrElse(throw new NoSuchElementException("No sibling " + field))

  def replace(newValue: JAtom): JAtomZipper
  def replace(newValue: JArray): JArrayZipper
  def replace(newValue: JObject): JObjectZipper

  def replace(newValue: JValue): JsonZipper = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }
}

sealed trait NothingZipper extends ZipperLike {
  def top : Option[JsonZipper]
  def top_! : JsonZipper
}

sealed trait JsonZipper extends ZipperLike {
  type ValueType <: JValue

  def value: ValueType

  def top : JsonZipper

  def remove: NothingZipper

  def asAtom = this.cast[JAtomZipper]
  def asArray = this.cast[JArrayZipper]
  def asObject = this.cast[JObjectZipper]
}

sealed trait JAtomZipper extends JsonZipper {
  type ValueType = JAtom
}

object JAtomZipper {
  def apply(value: JAtom) = new TopLevelAtomZipper(value)
  def unapply(zipper: JsonZipper) = zipper.cast[JAtomZipper].map(_.value)
}

sealed trait JArrayZipper extends JsonZipper {
  type ValueType = JArray

  private def subZipper(elem: JValue, idx: Int): JsonZipper = elem match {
    case atom: JAtom => new UnchangedArrayElementAtomZipper(atom, this, idx)
    case array: JArray => new UnchangedArrayElementArrayZipper(array, this, idx)
    case obj: JObject => new UnchangedArrayElementObjectZipper(obj, this, idx)
  }

  def down(idx: Int): Option[JsonZipper] =
    if(idx < 0 || idx >= length) None
    else Some(subZipper(value(idx), idx))

  def down_!(idx: Int): JsonZipper =
    subZipper(value(idx), idx)

  def first = down(0)
  def first_! = down_!(0)
  def last = down(size - 1)
  def last_! = down_!(size - 1)

  def remove(idx: Int) = down(idx) match {
    case Some(child) => child.remove.up_!.asInstanceOf[JArrayZipper]
    case None => None
  }

  def map(f: JValue => JValue): JArrayZipper
  def collect(f: PartialFunction[JValue, JValue]): JArrayZipper

  def set(idx: Int, value: JValue): JArrayZipper = down_!(idx).replace(value).up_!.asInstanceOf[JArrayZipper]
  def :+(value: JValue): JArrayZipper
  def +:(value: JValue): JArrayZipper

  def isEmpty = value.isEmpty
  def nonEmpty = value.nonEmpty

  lazy val size = value.size
  def length = size
}

object JArrayZipper {
  def apply(value: JArray): JArrayZipper = new TopLevelArrayZipper(value)
  def unapply(zipper: JsonZipper) = zipper.cast[JArrayZipper].map(_.value)
}

sealed trait JObjectZipper extends JsonZipper {
  type ValueType = JObject

  private def subZipper(elem: JValue, field: String): JsonZipper = elem match {
    case atom: JAtom => new UnchangedObjectElementAtomZipper(atom, this, field)
    case array: JArray => new UnchangedObjectElementArrayZipper(array, this, field)
    case obj: JObject => new UnchangedObjectElementObjectZipper(obj, this, field)
  }

  def down(field: String): Option[JsonZipper] = value.get(field) match {
    case Some(elem) => Some(subZipper(elem, field))
    case None => None
  }

  def down_!(field: String): JsonZipper = subZipper(value(field), field)

  def remove(field: String): JObjectZipper =
    down(field) match {
      case Some(child) => child.remove.up_!.asInstanceOf[JObjectZipper]
      case None => this
    }

  def set(field: String, value: JValue): JObjectZipper

  def map(f: ((String, JValue)) => JValue): JObjectZipper

  def contains(f: String) = value.contains(f)
}

object JObjectZipper {
  def apply(value: JObject): JObjectZipper = new TopLevelObjectZipper(value)
  def unapply(zipper: JsonZipper) = zipper.cast[JObjectZipper].map(_.value)
}

object JsonZipper {
  def apply(value: JAtom) = JAtomZipper(value)
  def apply(value: JArray) = JArrayZipper(value)
  def apply(value: JObject) = JObjectZipper(value)
  def apply(value: JValue): JsonZipper = value match {
    case atom: JAtom => apply(atom)
    case array: JArray => apply(array)
    case obj: JObject => apply(obj)
  }

  def unapply(zipper: JsonZipper): Option[JValue] = Some(zipper.value)

  implicit def toCastable[T <: JsonZipper](x: T): com.rojoma.`json-impl`.DownCaster[T] = new com.rojoma.`json-impl`.DownCaster(x)
}

// TOP LEVEL

private[zipper] class TopLevelZipperLike { this: ZipperLike =>
  def up = None
  def up_! = throw new NoSuchElementException("Cannot go up from the top level")

  def next = None
  def prev = None

  def sibling(field: String) = None

  def replace(newValue: JAtom) = JAtomZipper(newValue)
  def replace(newValue: JArray) = JArrayZipper(newValue)
  def replace(newValue: JObject) = JObjectZipper(newValue)
}

private[zipper] object TopLevelNothingZipper extends TopLevelZipperLike with NothingZipper {
  def top = None
  def top_! = throw new NoSuchElementException("No top-level object")
}

private[zipper] class TopLevelZipper extends TopLevelZipperLike { this: JsonZipper =>
  def remove = TopLevelNothingZipper
  def top : JsonZipper = this
}

private[zipper] class TopLevelAtomZipper(val value: JAtom) extends TopLevelZipper with JAtomZipper
private[zipper] class TopLevelArrayZipper(val value: JArray) extends TopLevelZipper with JArrayZipper {
  def :+(newValue: JValue): JArrayZipper = new TopLevelArrayZipper(JArray(value.toSeq :+ newValue))
  def +:(newValue: JValue): JArrayZipper = new TopLevelArrayZipper(JArray(newValue +: value.toSeq))

  def map(f: JValue => JValue) = new TopLevelArrayZipper(JArray(value.toSeq.map(f)))
  def collect(f: PartialFunction[JValue, JValue]) = new TopLevelArrayZipper(JArray(value.toSeq.collect(f)))
}
private[zipper] class TopLevelObjectZipper(val value: JObject) extends TopLevelZipper with JObjectZipper {
  def set(newField: String, newValue: JValue): JObjectZipper = new TopLevelObjectZipper(JObject(value.fields + (newField -> newValue)))
  def map(f: ((String, JValue)) => JValue) = new TopLevelObjectZipper(JObject(value.fields.map { kv => (kv._1, f(kv)) }))
}

// CODE COMMON TO ALL "ELEMENT" ZIPPER(LIKE)S

private[zipper] abstract class ElementZipperLike[Parent <: JsonZipper](parent : Parent) { this: ZipperLike =>
  def up_! = parent
  def up = Some(up_!)
}

private[zipper] trait ElementZipper[Parent <: JsonZipper] { this: ElementZipperLike[Parent] with JsonZipper =>
  def top = {
    @tailrec
    def loop(last: JsonZipper, next: Option[JsonZipper]): JsonZipper = next match {
      case None => last
      case Some(p) => loop(p, p.up)
    }
    loop(this, up)
  }
}

private[zipper] trait ElementNothingZipper[Parent <: JsonZipper] { this: ElementZipperLike[Parent] with NothingZipper =>
  def top_! : JsonZipper = up_!.top
  def top = Some(top_!)
}

// ARRAY ELEMENT, CHANGED OR NOT

private[zipper] abstract class ArrayElementZipperLike(p: JArrayZipper, val idxInParent: Int) extends ElementZipperLike(p) { this: ZipperLike =>
  def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, up_!, idxInParent)
  def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, up_!, idxInParent)
  def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, up_!, idxInParent)

  def remove = new ChangedArrayElementNothingZipper(up_!, idxInParent)

  def next : Option[JsonZipper] = up_!.down(idxInParent + 1)
  def prev : Option[JsonZipper] = up_!.down(idxInParent - 1)

  def sibling(field: String) = None
}

private[zipper] abstract class ArrayElementZipper(p: JArrayZipper, i: Int) extends ArrayElementZipperLike(p, i) with ElementZipper[JArrayZipper] { this: JsonZipper =>
}

private[zipper] trait ArrayElementArrayZipper { this: ArrayElementZipper with JArrayZipper =>
  def :+(newValue: JValue): JArrayZipper = new ChangedArrayElementArrayZipper(JArray(value.toSeq :+ newValue), up_!, idxInParent)
  def +:(newValue: JValue): JArrayZipper = new ChangedArrayElementArrayZipper(JArray(newValue +: value.toSeq), up_!, idxInParent)

  def map(f: JValue => JValue) = new ChangedArrayElementArrayZipper(JArray(value.toSeq.map(f)), up_!, idxInParent)
  def collect(f: PartialFunction[JValue, JValue]) = new ChangedArrayElementArrayZipper(JArray(value.toSeq.collect(f)), up_!, idxInParent)
}

private[zipper] trait ArrayElementObjectZipper { this: ArrayElementZipper with JObjectZipper =>
  def set(newField: String, newValue: JValue): JObjectZipper = new ChangedArrayElementObjectZipper(JObject(value.fields + (newField -> newValue)), up_!, idxInParent)

  def map(f: ((String, JValue)) => JValue) = new ChangedArrayElementObjectZipper(JObject(value.fields.map { kv => (kv._1, f(kv)) }), up_!, idxInParent)
}

// OBJECT ELEMENT, CHANGED OR NOT

private[zipper] abstract class ObjectElementZipperLike(p: JObjectZipper, val fieldInParent: String) extends ElementZipperLike(p) { this: ZipperLike =>
  def replace(newValue: JAtom) = new ChangedObjectElementAtomZipper(newValue, up_!, fieldInParent)
  def replace(newValue: JArray) = new ChangedObjectElementArrayZipper(newValue, up_!, fieldInParent)
  def replace(newValue: JObject) = new ChangedObjectElementObjectZipper(newValue, up_!, fieldInParent)

  def remove = new ChangedObjectElementNothingZipper(up_!, fieldInParent)

  def next = None
  def prev = None

  def sibling(field: String): Option[JsonZipper] = up_!.down(field)
}

private[zipper] abstract class ObjectElementZipper(p: JObjectZipper, f: String) extends ObjectElementZipperLike(p, f) with ElementZipper[JObjectZipper] { this: JsonZipper =>
}

private[zipper] trait ObjectElementArrayZipper { this: ObjectElementZipper with JArrayZipper =>
  def :+(newValue: JValue): JArrayZipper = new ChangedObjectElementArrayZipper(JArray(value.toSeq :+ newValue), up_!, fieldInParent)
  def +:(newValue: JValue): JArrayZipper = new ChangedObjectElementArrayZipper(JArray(newValue +: value.toSeq), up_!, fieldInParent)

  def map(f: JValue => JValue) = new ChangedObjectElementArrayZipper(JArray(value.toSeq.map(f)), up_!, fieldInParent)
  def collect(f: PartialFunction[JValue, JValue]) = new ChangedObjectElementArrayZipper(JArray(value.toSeq.collect(f)), up_!, fieldInParent)
}

private[zipper] trait ObjectElementObjectZipper { this: ObjectElementZipper with JObjectZipper =>
  def set(newField: String, newValue: JValue): JObjectZipper = new ChangedObjectElementObjectZipper(JObject(value.fields + (newField -> newValue)), up_!, fieldInParent)

  def map(f: ((String, JValue)) => JValue) = new ChangedObjectElementObjectZipper(JObject(value.fields.map { kv => (kv._1, f(kv)) }), up_!, fieldInParent)
}

// ARRAY ELEMENT BUT UNCHANGED

private[zipper] abstract class UnchangedArrayElementZipper(p: JArrayZipper, i: Int) extends ArrayElementZipper(p, i) { this: JsonZipper =>
}

private[zipper] class UnchangedArrayElementAtomZipper(val value: JAtom, p: JArrayZipper, i: Int) extends UnchangedArrayElementZipper(p, i) with JAtomZipper
private[zipper] class UnchangedArrayElementArrayZipper(val value: JArray, p: JArrayZipper, i: Int) extends UnchangedArrayElementZipper(p, i) with ArrayElementArrayZipper with JArrayZipper
private[zipper] class UnchangedArrayElementObjectZipper(val value: JObject, p: JArrayZipper, i: Int) extends UnchangedArrayElementZipper(p, i) with ArrayElementObjectZipper with JObjectZipper

// OBJECT ELEMENT BUT UNCHANGED

private[zipper] abstract class UnchangedObjectElementZipper(p: JObjectZipper, f: String) extends ObjectElementZipper(p, f) { this: JsonZipper =>
}

private[zipper] class UnchangedObjectElementAtomZipper(val value: JAtom, p: JObjectZipper, f: String) extends UnchangedObjectElementZipper(p, f) with JAtomZipper
private[zipper] class UnchangedObjectElementArrayZipper(val value: JArray, p: JObjectZipper, f: String) extends UnchangedObjectElementZipper(p, f) with ObjectElementArrayZipper with JArrayZipper
private[zipper] class UnchangedObjectElementObjectZipper(val value: JObject, p: JObjectZipper, f: String) extends UnchangedObjectElementZipper(p, f) with ObjectElementObjectZipper with JObjectZipper

// ARRAY ELEMENT AND CHANGED

private[zipper] abstract class ChangedArrayElementZipper(p: JArrayZipper, i: Int) extends ArrayElementZipper(p, i) { this: JsonZipper =>
  override def up_! = super.up_!.replace(JArray(super.up_!.value.toSeq.updated(idxInParent, value)))
}

private[zipper] class ChangedArrayElementAtomZipper(val value: JAtom, p: JArrayZipper, i: Int) extends ChangedArrayElementZipper(p, i) with JAtomZipper
private[zipper] class ChangedArrayElementArrayZipper(val value: JArray, p: JArrayZipper, i: Int) extends ChangedArrayElementZipper(p, i) with ArrayElementArrayZipper with JArrayZipper
private[zipper] class ChangedArrayElementObjectZipper(val value: JObject, p: JArrayZipper, i: Int) extends ChangedArrayElementZipper(p, i) with ArrayElementObjectZipper with JObjectZipper

private[zipper] class ChangedArrayElementNothingZipper(parent: JArrayZipper, i: Int) extends ArrayElementZipperLike(parent, i) with ElementNothingZipper[JArrayZipper] with NothingZipper {
  override def up_! = super.up_!.replace(JArray(super.up_!.value.toSeq.patch(idxInParent, Nil, 1)))

  override def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, parent, idxInParent)
  override def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, parent, idxInParent)
  override def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, parent, idxInParent)

  override def remove = this

  override def next : Option[JsonZipper] = up_!.down(idxInParent)
}

// OBJECT ELEMENT AND CHANGED

private[zipper] abstract class ChangedObjectElementZipper(p: JObjectZipper, f: String) extends ObjectElementZipper(p, f) with ElementZipper[JObjectZipper] { this: JsonZipper =>
  override def up_! = super.up_!.replace(JObject(super.up_!.value.fields.updated(fieldInParent, value)))
}

private[zipper] class ChangedObjectElementAtomZipper(val value: JAtom, p: JObjectZipper, f: String) extends ChangedObjectElementZipper(p, f) with JAtomZipper
private[zipper] class ChangedObjectElementArrayZipper(val value: JArray, p: JObjectZipper, f: String) extends ChangedObjectElementZipper(p, f) with ObjectElementArrayZipper with JArrayZipper
private[zipper] class ChangedObjectElementObjectZipper(val value: JObject, p: JObjectZipper, f: String) extends ChangedObjectElementZipper(p, f) with ObjectElementObjectZipper with JObjectZipper

private[zipper] class ChangedObjectElementNothingZipper(parent: JObjectZipper, field: String) extends ObjectElementZipperLike(parent, field) with ElementNothingZipper[JObjectZipper] with NothingZipper {
  override def up_! = super.up_!.replace(JObject(super.up_!.value.fields - field))
}
