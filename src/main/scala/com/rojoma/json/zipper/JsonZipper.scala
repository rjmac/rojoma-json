package com.rojoma.json
package zipper

import ast._

sealed trait ZipperLike[Parent] {
  def up(implicit ev: Parent <:< JCompoundZipper[_]): Parent
  def upOpt: Option[JsonZipper[_]]

  def replace(newValue: JAtom): JAtomZipper[Parent]
  def replace(newValue: JArray): JArrayZipper[Parent]
  def replace(newValue: JObject): JObjectZipper[Parent]

  def replace(newValue: JValue): JsonZipper[Parent] = newValue match {
    case atom: JAtom => replace(atom)
    case array: JArray => replace(array)
    case obj: JObject => replace(obj)
  }
}

sealed trait NothingZipper[Parent] extends ZipperLike[Parent] {
  def top: Option[JsonZipper[Root]]
}

sealed trait JsonZipper[Parent] extends ZipperLike[Parent] {
  def top: JsonZipper[Root]
  def here: JValue

  def remove: NothingZipper[Parent]

  def asAtom = this.cast[JAtomZipper[Parent]]
  def asArray = this.cast[JArrayZipper[Parent]]
  def asObject = this.cast[JObjectZipper[Parent]]
}

sealed trait JAtomZipper[Parent] extends JsonZipper[Parent] {
  def here: JAtom
}

object JAtomZipper {
  def unapply[P](zipper: JsonZipper[P]) = zipper.cast[JAtomZipper[P]]
}

sealed trait JCompoundZipper[Parent] extends JsonZipper[Parent] {
  def here: JCompound
}

sealed trait JArrayZipper[Parent] extends JCompoundZipper[Parent] {
  def here: JArray

  private def subZipper(elem: JValue, idx: Int) = elem match {
    case atom: JAtom => new UnchangedArrayElementAtomZipper[JArrayZipper[Parent]](atom, this, idx)
    case array: JArray => new UnchangedArrayElementArrayZipper[JArrayZipper[Parent]](array, this, idx)
    case obj: JObject => new UnchangedArrayElementObjectZipper[JArrayZipper[Parent]](obj, this, idx)
  }

  def down(idx: Int): JsonZipper[JArrayZipper[Parent]] = subZipper(here(idx), idx)

  def map[P](f: JsonZipper[JArrayZipper[Parent]] => ZipperLike[JArrayZipper[Parent]]) = {
    here.toSeq.view.zipWithIndex.foldLeft(this : JArrayZipper[Parent]) { (newSelf, childIdx) =>
      val (child, idx) = childIdx
      f(newSelf.subZipper(child, idx)).up
    }
  }

  def remove(idx: Int): JArrayZipper[Parent]
  def size = here.size
  def length = size
}

object JArrayZipper {
  def unapply[P](zipper: JsonZipper[P]) = zipper.cast[JArrayZipper[P]]
}

sealed trait JObjectZipper[Parent] extends JCompoundZipper[Parent] {
  def here: JObject

  private def subZipper(elem: JValue, field: String) = elem match {
    case atom: JAtom => new UnchangedObjectElementAtomZipper[JObjectZipper[Parent]](atom, this, field)
    case array: JArray => new UnchangedObjectElementArrayZipper[JObjectZipper[Parent]](array, this, field)
    case obj: JObject => new UnchangedObjectElementObjectZipper[JObjectZipper[Parent]](obj, this, field)
  }

  def down(field: String): JsonZipper[JObjectZipper[Parent]] = subZipper(here(field), field)
  def remove(field: String): JObjectZipper[Parent]

  def map(f: (String, JsonZipper[JObjectZipper[Parent]]) => ZipperLike[JObjectZipper[Parent]]) = {
    here.fields.foldLeft(this : JObjectZipper[Parent]) { (newSelf, fieldChild) =>
      val (field, child) = fieldChild
      f(field, newSelf.subZipper(child, field)).up
    }
  }
}

object JObjectZipper {
  def unapply[P](zipper: JsonZipper[P]) = zipper.cast[JObjectZipper[P]]
}

object JsonZipper {
  def apply(value: JAtom): JAtomZipper[Root] = new TopLevelAtomZipper(value)
  def apply(value: JArray): JArrayZipper[Root] = new TopLevelArrayZipper(value)
  def apply(value: JObject): JObjectZipper[Root] = new TopLevelObjectZipper(value)
  def apply(value: JValue): JsonZipper[Root] = value match {
    case atom: JAtom => apply(atom)
    case array: JArray => apply(array)
    case obj: JObject => apply(obj)
  }

  implicit def toCastable[T <: JsonZipper[_]](x: T): com.rojoma.`json-impl`.DownCaster[T] = new com.rojoma.`json-impl`.DownCaster(x)
}

// TOP LEVEL

sealed abstract trait Root // phantom

private[zipper] class TopLevelZipperLike {
  def up(implicit ev: Root <:< JCompoundZipper[_]) = throw new Exception("Can't go up from the top")
  def upOpt = None

  def replace(newValue: JAtom) = new TopLevelAtomZipper(newValue)
  def replace(newValue: JArray) = new TopLevelArrayZipper(newValue)
  def replace(newValue: JObject) = new TopLevelObjectZipper(newValue)
}

private[zipper] object TopLevelNothingZipper extends TopLevelZipperLike with NothingZipper[Root] {
  def top = None
}

private[zipper] class TopLevelZipper extends TopLevelZipperLike {
  def remove = TopLevelNothingZipper
  def top: this.type = this
}

private[zipper] class TopLevelAtomZipper(val here: JAtom) extends TopLevelZipper with JAtomZipper[Root]

private[zipper] class TopLevelArrayZipper(val here: JArray) extends TopLevelZipper with JArrayZipper[Root] {
  def remove(idx: Int) = new TopLevelArrayZipper(JArray(here.toSeq.patch(idx, Nil, 1)))
}

private[zipper] class TopLevelObjectZipper(val here: JObject) extends TopLevelZipper with JObjectZipper[Root] {
  def remove(field: String) = new TopLevelObjectZipper(JObject(here.fields - field))
}

// ARRAY ELEMENT BUT UNCHANGED

private[zipper] abstract class ElementZipperLike[Parent <: JCompoundZipper[_]](val parent: Parent) {
  def up(implicit ev: Parent <:< JCompoundZipper[_]) = parent
  def upOpt = Some(up)
}

private[zipper] trait ElementZipper[Parent <: JCompoundZipper[_]] extends JsonZipper[Parent] { this: ElementZipperLike[Parent] =>
  def top = {
    def loop(last: JsonZipper[_], next: Option[JsonZipper[_]]): JsonZipper[_] = {
      next match {
        case None => last
        case Some(p) => loop(p, p.upOpt)
      }
    }
    loop(this, upOpt).asInstanceOf[JsonZipper[Root]]
  }
}

private[zipper] abstract class ArrayElementZipperLike[Parent <: JArrayZipper[_]](p: Parent, val idx: Int) extends ElementZipperLike(p) {
  def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, parent, idx)
  def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, parent, idx)
  def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, parent, idx)

  def remove = new ChangedArrayElementNothingZipper(parent, idx)
}

private[zipper] abstract class ArrayElementZipper[Parent <: JArrayZipper[_]](p: Parent, i: Int) extends ArrayElementZipperLike(p, i)  with ElementZipper[Parent]

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

private[zipper] abstract class ObjectElementZipperLike[Parent <: JObjectZipper[_]](p: Parent, val field: String) extends ElementZipperLike(p) {
  def replace(newValue: JAtom) = new ChangedObjectElementAtomZipper(newValue, parent, field)
  def replace(newValue: JArray) = new ChangedObjectElementArrayZipper(newValue, parent, field)
  def replace(newValue: JObject) = new ChangedObjectElementObjectZipper(newValue, parent, field)

  def remove = new ChangedObjectElementNothingZipper(parent, field)
}

private[zipper] abstract class ObjectElementZipper[Parent <: JObjectZipper[_]](p: Parent, f: String) extends ObjectElementZipperLike(p, f) with ElementZipper[Parent]

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
  override def up(implicit ev: Parent <:< JCompoundZipper[_]) = parent.replace(JArray(parent.here.toSeq.updated(idx, here))).asInstanceOf[Parent]
}

private[zipper] class ChangedArrayElementAtomZipper[Parent <: JArrayZipper[_]](val here: JAtom, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with JAtomZipper[Parent]
private[zipper] class ChangedArrayElementArrayZipper[Parent <: JArrayZipper[_]](val here: JArray, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementArrayZipper[Parent]
private[zipper] class ChangedArrayElementObjectZipper[Parent <: JArrayZipper[_]](val here: JObject, parent: Parent, idx: Int) extends ChangedArrayElementZipper(parent, idx) with ArrayElementObjectZipper[Parent]

private[zipper] class ChangedArrayElementNothingZipper[Parent <: JArrayZipper[_]](parent: Parent, idx: Int) extends ArrayElementZipperLike(parent, idx) with NothingZipper[Parent] {
  override def up(implicit ev: Parent <:< JCompoundZipper[_]) = parent.replace(JArray(parent.here.toSeq.patch(idx, Nil, 1))).asInstanceOf[Parent]
  def top = Some(up.top)
}

// OBJECT ELEMENT AND CHANGED

private[zipper] abstract class ChangedObjectElementZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipper(parent, field) with ElementZipper[Parent] {
  def here: JValue
  override def up(implicit ev: Parent <:< JCompoundZipper[_]) = parent.replace(JObject(parent.here.fields.updated(field, here))).asInstanceOf[Parent]
}

private[zipper] class ChangedObjectElementAtomZipper[Parent <: JObjectZipper[_]](val here: JAtom, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with JAtomZipper[Parent]
private[zipper] class ChangedObjectElementArrayZipper[Parent <: JObjectZipper[_]](val here: JArray, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementArrayZipper[Parent]
private[zipper] class ChangedObjectElementObjectZipper[Parent <: JObjectZipper[_]](val here: JObject, parent: Parent, field: String) extends ChangedObjectElementZipper(parent, field) with ObjectElementObjectZipper[Parent]

private[zipper] class ChangedObjectElementNothingZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipperLike(parent, field) with NothingZipper[Parent] {
  override def up(implicit ev: Parent <:< JCompoundZipper[_]) = parent.replace(JObject(parent.here.fields - field)).asInstanceOf[Parent]
  override def top = Some(up.top)
}
