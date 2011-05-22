package com.rojoma.json
package zipper

import ast._

sealed trait NothingZipper {
  def top: Option[JsonZipper] = up match {
    case None => None
    case Some(x) => Some(x.top)
  }

  def up: Option[JCompoundZipper]

  def replace(newValue: JAtom): JAtomZipper
  def replace(newValue: JArray): JArrayZipper
  def replace(newValue: JObject): JObjectZipper

  def replace(newValue: JValue): JsonZipper = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }
}

sealed trait JsonZipper {
  def up: Option[JCompoundZipper]
  def top: JsonZipper
  def here: JValue

  def replace(newValue: JAtom): JAtomZipper
  def replace(newValue: JArray): JArrayZipper
  def replace(newValue: JObject): JObjectZipper

  def replace(newValue: JValue): JsonZipper = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }

  def remove: NothingZipper
}

sealed trait JAtomZipper extends JsonZipper {
  def top = up match {
    case None =>
      this
    case Some(parent) =>
      def loop(ancestor: JsonZipper): JsonZipper =
        ancestor.up match {
          case None => ancestor
          case Some(ancestorer) => loop(ancestorer)
        }
      loop(parent)
  }
  def here: JAtom
}

sealed trait JCompoundZipper extends JsonZipper {
  def here: JCompound
  def top: JCompoundZipper = up match {
    case None =>
      this
    case Some(parent) =>
      def loop(ancestor: JCompoundZipper): JCompoundZipper =
        ancestor.up match {
          case None => ancestor
          case Some(ancestorer) => loop(ancestorer)
        }
      loop(parent)
  }
}

sealed trait JArrayZipper extends JCompoundZipper {
  def here: JArray

  def down(idx: Int) = here(idx) match {
    case atom: JAtom => new UnchangedArrayElementAtomZipper(atom, this, idx)
    case array: JArray => new UnchangedArrayElementArrayZipper(array, this, idx)
    case obj: JObject => new UnchangedArrayElementObjectZipper(obj, this, idx)
  }

  def remove(idx: Int): JArrayZipper
  def size = here.size
  def length = size
}

sealed trait JObjectZipper extends JCompoundZipper {
  def here: JObject
  def down(field: String): JsonZipper = here(field) match {
    case atom: JAtom => new UnchangedObjectElementAtomZipper(atom, this, field)
    case array: JArray => new UnchangedObjectElementArrayZipper(array, this, field)
    case obj: JObject => new UnchangedObjectElementObjectZipper(obj, this, field)
  }
  def remove(field: String): JObjectZipper
}

object JsonZipper {
  def apply(value: JAtom): JAtomZipper = new TopLevelAtomZipper(value)
  def apply(value: JArray): JArrayZipper = new TopLevelArrayZipper(value)
  def apply(value: JObject): JObjectZipper = new TopLevelObjectZipper(value)
  def apply(value: JValue): JsonZipper = value match {
    case atom: JAtom => apply(atom)
    case array: JArray => apply(array)
    case obj: JObject => apply(obj)
  }

  implicit def toCastable[T <: JsonZipper](x: T) = new com.rojoma.`json-impl`.DownCaster(x)
}

// TOP LEVEL

private[zipper] class TopLevelZipperLike {
  def up = None

  def replace(newValue: JAtom) = new TopLevelAtomZipper(newValue)
  def replace(newValue: JArray) = new TopLevelArrayZipper(newValue)
  def replace(newValue: JObject) = new TopLevelObjectZipper(newValue)
}

private[zipper] object TopLevelNothingZipper extends TopLevelZipperLike with NothingZipper

private[zipper] class TopLevelZipper extends TopLevelZipperLike {
  def remove = TopLevelNothingZipper
}

private[zipper] class TopLevelAtomZipper(val here: JAtom) extends TopLevelZipper with JAtomZipper

private[zipper] class TopLevelArrayZipper(val here: JArray) extends TopLevelZipper with JArrayZipper {
  def remove(idx: Int) = new TopLevelArrayZipper(JArray(here.toSeq.patch(idx, Nil, 1)))
}

private[zipper] class TopLevelObjectZipper(val here: JObject) extends TopLevelZipper with JObjectZipper {
  def remove(field: String) = new TopLevelObjectZipper(JObject(here.fields - field))
}

// ARRAY ELEMENT BUT UNCHANGED

private[zipper] abstract class ArrayElementZipper(val parent: JArrayZipper, val idx: Int) {
  def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, parent, idx)
  def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, parent, idx)
  def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, parent, idx)

  def remove = new ChangedArrayElementNothingZipper(parent, idx)
}

private[zipper] trait ArrayElementArrayZipper extends JArrayZipper { this: ArrayElementZipper =>
  def remove(idx: Int) = new ChangedArrayElementArrayZipper(JArray(here.toSeq.patch(idx, Nil, 1)), parent, idx)
}

private[zipper] trait ArrayElementObjectZipper extends JObjectZipper { this: ArrayElementZipper =>
  def remove(field: String) = new ChangedArrayElementObjectZipper(JObject(here.fields - field), parent, idx)
}

private[zipper] abstract class UnchangedArrayElementZipper(parent: JArrayZipper, idx: Int) extends ArrayElementZipper(parent, idx) {
  def up = Some(parent)
}

private[zipper] class UnchangedArrayElementAtomZipper(val here: JAtom, parent: JArrayZipper, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with JAtomZipper
private[zipper] class UnchangedArrayElementArrayZipper(val here: JArray, parent: JArrayZipper, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with ArrayElementArrayZipper
private[zipper] class UnchangedArrayElementObjectZipper(val here: JObject, parent: JArrayZipper, idx: Int) extends UnchangedArrayElementZipper(parent, idx) with ArrayElementObjectZipper

// OBJECT ELEMENT BUT UNCHANGED

private[zipper] abstract class ObjectElementZipper(val parent: JObjectZipper, val field: String) {
  def replace(newValue: JAtom) = new ChangedObjectElementAtomZipper(newValue, parent, field)
  def replace(newValue: JArray) = new ChangedObjectElementArrayZipper(newValue, parent, field)
  def replace(newValue: JObject) = new ChangedObjectElementObjectZipper(newValue, parent, field)
  def remove = new ChangedObjectElementNothingZipper(parent, field)
}

private[zipper] trait ObjectElementArrayZipper extends JArrayZipper { this: ObjectElementZipper =>
  def remove(idx: Int) = new ChangedObjectElementArrayZipper(JArray(here.toSeq.patch(idx, Nil, 1)), parent, field)
}

private[zipper] trait ObjectElementObjectZipper extends JObjectZipper { this: ObjectElementZipper =>
  def remove(field: String) = new ChangedObjectElementObjectZipper(JObject(here.fields - field), parent, field)
}

private[zipper] abstract class UnchangedObjectElementZipper(parent: JObjectZipper, field: String) extends ObjectElementZipper(parent, field) {
  def up = Some(parent)
}

private[zipper] class UnchangedObjectElementAtomZipper(val here: JAtom, parent: JObjectZipper, field: String) extends UnchangedObjectElementZipper(parent, field) with JAtomZipper
private[zipper] class UnchangedObjectElementArrayZipper(val here: JArray, parent: JObjectZipper, field: String) extends UnchangedObjectElementZipper(parent, field) with ObjectElementArrayZipper
private[zipper] class UnchangedObjectElementObjectZipper(val here: JObject, parent: JObjectZipper, field: String) extends UnchangedObjectElementZipper(parent, field) with ObjectElementObjectZipper

// ARRAY ELEMENT AND CHANGED

private[zipper] abstract class ChangedArrayElementZipper(parent: JArrayZipper, idx: Int) extends ArrayElementZipper(parent, idx) {
  def here: JValue
  def up = Some(parent.replace(JArray(parent.here.toSeq.updated(idx, here))))
}

private[zipper] class ChangedArrayElementAtomZipper(val here: JAtom, parent: JArrayZipper, idx: Int) extends ChangedArrayElementZipper(parent, idx) with JAtomZipper
private[zipper] class ChangedArrayElementArrayZipper(val here: JArray, parent: JArrayZipper, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementArrayZipper
private[zipper] class ChangedArrayElementObjectZipper(val here: JObject, parent: JArrayZipper, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementObjectZipper

private[zipper] class ChangedArrayElementNothingZipper(parent: JArrayZipper, idx: Int) extends ArrayElementZipper(parent, idx) with NothingZipper {
  def up = Some(parent.replace(JArray(parent.here.toSeq.patch(idx, Nil, 1))))
}

// OBJECT ELEMENT AND CHANGED

private[zipper] abstract class ChangedObjectElementZipper(parent: JObjectZipper, field: String) extends ObjectElementZipper(parent, field) {
  def here: JValue
  def up = Some(parent.replace(JObject(parent.here.fields.updated(field, here))))
}

private[zipper] class ChangedObjectElementAtomZipper(val here: JAtom, parent: JObjectZipper, field: String) extends ChangedObjectElementZipper(parent, field) with JAtomZipper
private[zipper] class ChangedObjectElementArrayZipper(val here: JArray, parent: JObjectZipper, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementArrayZipper
private[zipper] class ChangedObjectElementObjectZipper(val here: JObject, parent: JObjectZipper, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementObjectZipper

private[zipper] class ChangedObjectElementNothingZipper(parent: JObjectZipper, field: String) extends ObjectElementZipper(parent, field) with NothingZipper {
  def up = Some(parent.replace(JObject(parent.here.fields - field)))
}
