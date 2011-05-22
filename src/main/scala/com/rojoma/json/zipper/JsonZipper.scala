package com.rojoma.json
package zipper

import ast._

sealed trait NothingZipper[Parent <: JCompoundZipper[_]] {
  def top: Option[JsonZipper[Nothing]]

  def up: Parent

  def replace(newValue: JAtom): JAtomZipper[Parent]
  def replace(newValue: JArray): JArrayZipper[Parent]
  def replace(newValue: JObject): JObjectZipper[Parent]

  def replace(newValue: JValue): JsonZipper[Parent] = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }
}

sealed trait JsonZipper[Parent <: JCompoundZipper[_]] {
  def up: Parent
  def top: JsonZipper[Nothing]
  def here: JValue

  def replace(newValue: JAtom): JAtomZipper[Parent]
  def replace(newValue: JArray): JArrayZipper[Parent]
  def replace(newValue: JObject): JObjectZipper[Parent]

  def replace(newValue: JValue): JsonZipper[Parent] = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }

  def remove: NothingZipper[Parent]

  def asObject = this.cast[JObjectZipper[Parent]]
  def asArray = this.cast[JArrayZipper[Parent]]
}

sealed trait JAtomZipper[Parent <: JCompoundZipper[_]] extends JsonZipper[Parent] {
  def here: JAtom
}

sealed trait JCompoundZipper[Parent <: JCompoundZipper[_]] extends JsonZipper[Parent] {
  def here: JCompound
}

sealed trait JArrayZipper[Parent <: JCompoundZipper[_]] extends JCompoundZipper[Parent] {
  def here: JArray

  def down(idx: Int): JsonZipper[JArrayZipper[Parent]] = here(idx) match {
    case atom: JAtom => new UnchangedArrayElementAtomZipper[JArrayZipper[Parent]](atom, this, idx)
    case array: JArray => new UnchangedArrayElementArrayZipper[JArrayZipper[Parent]](array, this, idx)
    case obj: JObject => new UnchangedArrayElementObjectZipper[JArrayZipper[Parent]](obj, this, idx)
  }

  def remove(idx: Int): JArrayZipper[Parent]
  def size = here.size
  def length = size
}

sealed trait JObjectZipper[Parent <: JCompoundZipper[_]] extends JCompoundZipper[Parent] {
  def here: JObject
  def down(field: String): JsonZipper[JObjectZipper[Parent]] = here(field) match {
    case atom: JAtom => new UnchangedObjectElementAtomZipper[JObjectZipper[Parent]](atom, this, field)
    case array: JArray => new UnchangedObjectElementArrayZipper[JObjectZipper[Parent]](array, this, field)
    case obj: JObject => new UnchangedObjectElementObjectZipper[JObjectZipper[Parent]](obj, this, field)
  }
  def remove(field: String): JObjectZipper[Parent]
}

object JsonZipper {
  def apply(value: JAtom): JAtomZipper[Nothing] = new TopLevelAtomZipper(value)
  def apply(value: JArray): JArrayZipper[Nothing] = new TopLevelArrayZipper(value)
  def apply(value: JObject): JObjectZipper[Nothing] = new TopLevelObjectZipper(value)
  def apply(value: JValue): JsonZipper[Nothing] = value match {
    case atom: JAtom => apply(atom)
    case array: JArray => apply(array)
    case obj: JObject => apply(obj)
  }

  implicit def toCastable[T <: JsonZipper[_]](x: T): com.rojoma.`json-impl`.DownCaster[T] = new com.rojoma.`json-impl`.DownCaster(x)
}

// TOP LEVEL

private[zipper] class TopLevelZipperLike {
  def up = throw new Exception("Can't go up from the top")

  def replace(newValue: JAtom) = new TopLevelAtomZipper(newValue)
  def replace(newValue: JArray) = new TopLevelArrayZipper(newValue)
  def replace(newValue: JObject) = new TopLevelObjectZipper(newValue)
}

private[zipper] object TopLevelNothingZipper extends TopLevelZipperLike with NothingZipper[Nothing] {
  def top = None
}

private[zipper] class TopLevelZipper extends TopLevelZipperLike {
  def remove = TopLevelNothingZipper
  def top: this.type = this
}

private[zipper] class TopLevelAtomZipper(val here: JAtom) extends TopLevelZipper with JAtomZipper[Nothing]

private[zipper] class TopLevelArrayZipper(val here: JArray) extends TopLevelZipper with JArrayZipper[Nothing] {
  def remove(idx: Int) = new TopLevelArrayZipper(JArray(here.toSeq.patch(idx, Nil, 1)))
}

private[zipper] class TopLevelObjectZipper(val here: JObject) extends TopLevelZipper with JObjectZipper[Nothing] {
  def remove(field: String) = new TopLevelObjectZipper(JObject(here.fields - field))
}

// ARRAY ELEMENT BUT UNCHANGED

private[zipper] abstract class ArrayElementZipperLike[Parent <: JArrayZipper[_]](val parent: Parent, val idx: Int) {
  def up = parent
  def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, parent, idx)
  def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, parent, idx)
  def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, parent, idx)

  def remove = new ChangedArrayElementNothingZipper(parent, idx)
}

private[zipper] abstract class ArrayElementZipper[Parent <: JArrayZipper[_]](parent: Parent, idx: Int) extends ArrayElementZipperLike(parent, idx) {
  def top = parent.top
}

private[zipper] trait ArrayElementArrayZipper[Parent <: JArrayZipper[_]] extends JArrayZipper[Parent] { this: ArrayElementZipper[Parent] =>
  def remove(removed: Int) = new ChangedArrayElementArrayZipper(JArray(here.toSeq.patch(removed, Nil, 1)), parent, idx)
}

private[zipper] trait ArrayElementObjectZipper[Parent <: JArrayZipper[_]] extends JObjectZipper[Parent] { this: ArrayElementZipper[Parent] =>
  def remove(removed: String) = new ChangedArrayElementObjectZipper(JObject(here.fields - removed), parent, idx)
}

private[zipper] abstract class UnchangedArrayElementZipper[Parent <: JArrayZipper[_]](parent: Parent, idx: Int) extends ArrayElementZipper(parent, idx)

private[zipper] class UnchangedArrayElementAtomZipper[Parent <: JArrayZipper[_]](val here: JAtom, parent: Parent, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with JAtomZipper[Parent]
private[zipper] class UnchangedArrayElementArrayZipper[Parent <: JArrayZipper[_]](val here: JArray, parent: Parent, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with ArrayElementArrayZipper[Parent]
private[zipper] class UnchangedArrayElementObjectZipper[Parent <: JArrayZipper[_]](val here: JObject, parent: Parent, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with ArrayElementObjectZipper[Parent]

// OBJECT ELEMENT BUT UNCHANGED

private[zipper] abstract class ObjectElementZipperLike[Parent <: JObjectZipper[_]](val parent: Parent, val field: String) {
  def up = parent
  def replace(newValue: JAtom) = new ChangedObjectElementAtomZipper(newValue, parent, field)
  def replace(newValue: JArray) = new ChangedObjectElementArrayZipper(newValue, parent, field)
  def replace(newValue: JObject) = new ChangedObjectElementObjectZipper(newValue, parent, field)
  def remove = new ChangedObjectElementNothingZipper(parent, field)
}

private[zipper] abstract class ObjectElementZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipperLike(parent, field) {
  def top = parent.top
}

private[zipper] trait ObjectElementArrayZipper[Parent <: JObjectZipper[_]] extends JArrayZipper[Parent] { this: ObjectElementZipper[Parent] =>
  def remove(removed: Int) = new ChangedObjectElementArrayZipper(JArray(here.toSeq.patch(removed, Nil, 1)), parent, field)
}

private[zipper] trait ObjectElementObjectZipper[Parent <: JObjectZipper[_]] extends JObjectZipper[Parent] { this: ObjectElementZipper[Parent] =>
  def remove(removed: String) = new ChangedObjectElementObjectZipper(JObject(here.fields - removed), parent, field)
}

private[zipper] abstract class UnchangedObjectElementZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipper(parent, field)

private[zipper] class UnchangedObjectElementAtomZipper[Parent <: JObjectZipper[_]](val here: JAtom, parent: Parent, field: String) extends UnchangedObjectElementZipper(parent, field) with JAtomZipper[Parent]
private[zipper] class UnchangedObjectElementArrayZipper[Parent <: JObjectZipper[_]](val here: JArray, parent: Parent, field: String) extends UnchangedObjectElementZipper(parent, field) with ObjectElementArrayZipper[Parent]
private[zipper] class UnchangedObjectElementObjectZipper[Parent <: JObjectZipper[_]](val here: JObject, parent: Parent, field: String) extends UnchangedObjectElementZipper(parent, field) with ObjectElementObjectZipper[Parent]

// ARRAY ELEMENT AND CHANGED

private[zipper] abstract class ChangedArrayElementZipper[Parent <: JArrayZipper[_]](parent: Parent, idx: Int) extends ArrayElementZipper(parent, idx) {
  def here: JValue
  override def up = parent.replace(JArray(parent.here.toSeq.updated(idx, here))).asInstanceOf[Parent]
  override def top = up.top
}

private[zipper] class ChangedArrayElementAtomZipper[Parent <: JArrayZipper[_]](val here: JAtom, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with JAtomZipper[Parent]
private[zipper] class ChangedArrayElementArrayZipper[Parent <: JArrayZipper[_]](val here: JArray, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementArrayZipper[Parent]
private[zipper] class ChangedArrayElementObjectZipper[Parent <: JArrayZipper[_]](val here: JObject, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementObjectZipper[Parent]

private[zipper] class ChangedArrayElementNothingZipper[Parent <: JArrayZipper[_]](parent: Parent, idx: Int) extends ArrayElementZipperLike(parent, idx) with NothingZipper[Parent] {
  override def up = parent.replace(JArray(parent.here.toSeq.patch(idx, Nil, 1))).asInstanceOf[Parent]
  def top = Some(up.top)
}

// OBJECT ELEMENT AND CHANGED

private[zipper] abstract class ChangedObjectElementZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipper(parent, field) {
  def here: JValue
  override def up = parent.replace(JObject(parent.here.fields.updated(field, here))).asInstanceOf[Parent]
  override def top = up.top
}

private[zipper] class ChangedObjectElementAtomZipper[Parent <: JObjectZipper[_]](val here: JAtom, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with JAtomZipper[Parent]
private[zipper] class ChangedObjectElementArrayZipper[Parent <: JObjectZipper[_]](val here: JArray, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementArrayZipper[Parent]
private[zipper] class ChangedObjectElementObjectZipper[Parent <: JObjectZipper[_]](val here: JObject, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementObjectZipper[Parent]

private[zipper] class ChangedObjectElementNothingZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipperLike(parent, field) with NothingZipper[Parent] {
  override def up = parent.replace(JObject(parent.here.fields - field)).asInstanceOf[Parent]
  override def top = Some(up.top)
}
