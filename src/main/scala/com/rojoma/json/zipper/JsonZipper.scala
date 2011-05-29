package com.rojoma.json
package zipper

import ast._

sealed trait ZipperLike[Parent] {
  def top_? : Option[JsonZipper[_]]

  def up(implicit ev: Parent <:< JsonZipper[_]): Parent
  def up_? : Option[JsonZipper[_]]

  def next(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]]
  def prev(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]]

  def next_? : Option[JsonZipper[Parent]]
  def prev_? : Option[JsonZipper[Parent]]

  def position(implicit ev: Parent <:< JArrayZipper[_]): Int
  def field(implicit ev: Parent <:< JObjectZipper[_]): String

  def position_? : Option[Int]
  def field_? : Option[String]

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
  def top(implicit ev: Parent <:< JsonZipper[_]): JsonZipper[_]
}

sealed trait JsonZipper[Parent] extends ZipperLike[Parent] {
  type Self
  def here: JValue

  def top: JsonZipper[_]
  def top_? = Some(top)

  def down_?(idx: Int): Option[JsonZipper[Self]]
  def down_?(field: String): Option[JsonZipper[Self]]

  def first_? : Option[JsonZipper[Self]]
  def last_? : Option[JsonZipper[Self]]

  def remove: NothingZipper[Parent]

  def asAtom = this.cast[JAtomZipper[Parent]]
  def asCompound = this.cast[JCompoundZipper[Parent]]
  def asArray = this.cast[JArrayZipper[Parent]]
  def asObject = this.cast[JObjectZipper[Parent]]
}

sealed trait JAtomZipper[Parent] extends JsonZipper[Parent] {
  type Self = JAtomZipper[Parent]
  def here: JAtom

  def down_?(idx: Int): Option[JsonZipper[Self]] = None
  def down_?(field: String): Option[JsonZipper[Self]] = None

  def first_? : Option[JsonZipper[Self]] = None
  def last_? : Option[JsonZipper[Self]] = None
}

object JAtomZipper {
  def unapply[P](zipper: JsonZipper[P]) = zipper.cast[JAtomZipper[P]]
}

sealed trait JCompoundZipper[Parent] extends JsonZipper[Parent] {
  type Self <: JCompoundZipper[Parent]
  def here: JCompound

  def first: JsonZipper[Self]
  def last: JsonZipper[Self]
}

sealed trait JArrayZipper[Parent] extends JCompoundZipper[Parent] {
  type Self = JArrayZipper[Parent]
  def here: JArray

  private def subZipper(elem: JValue, idx: Int): JsonZipper[JArrayZipper[Parent]] = elem match {
    case atom: JAtom => new UnchangedArrayElementAtomZipper[JArrayZipper[Parent]](atom, this, idx)
    case array: JArray => new UnchangedArrayElementArrayZipper[JArrayZipper[Parent]](array, this, idx)
    case obj: JObject => new UnchangedArrayElementObjectZipper[JArrayZipper[Parent]](obj, this, idx)
  }

  def down(idx: Int)(implicit ev: Self <:< JArrayZipper[_]): JsonZipper[Self] = subZipper(here(idx), idx)

  def down_?(idx: Int): Option[JsonZipper[Self]] = {
    if(0 <= idx && here.toSeq.lengthCompare(idx) > 0) Some(down(idx))
    else None
  }
  def down_?(field: String): Option[JsonZipper[Self]] = None

  def first: JsonZipper[Self] = down(0)
  def last: JsonZipper[Self] = down(size - 1)

  def first_? : Option[JsonZipper[Self]] = if(!here.isEmpty) Some(first) else None
  def last_? : Option[JsonZipper[Self]] = if(!here.isEmpty) Some(last) else None

  def remove(idx: Int): Self = down(idx).remove.up

  def map[P](f: JsonZipper[JArrayZipper[Parent]] => ZipperLike[JArrayZipper[Parent]]) = {
    here.toSeq.view.zipWithIndex.foldLeft(this : JArrayZipper[Parent]) { (newSelf, childIdx) =>
      val (child, idx) = childIdx
      f(newSelf.subZipper(child, idx)).up
    }
  }

  def size = here.size
  def length = size
}

object JArrayZipper {
  def unapply[P](zipper: JsonZipper[P]) = zipper.cast[JArrayZipper[P]]
}

sealed trait JObjectZipper[Parent] extends JCompoundZipper[Parent] {
  def here: JObject

  type Self = JObjectZipper[Parent]

  private def subZipper(elem: JValue, field: String): JsonZipper[JObjectZipper[Parent]] = elem match {
    case atom: JAtom => new UnchangedObjectElementAtomZipper[JObjectZipper[Parent]](atom, this, field)
    case array: JArray => new UnchangedObjectElementArrayZipper[JObjectZipper[Parent]](array, this, field)
    case obj: JObject => new UnchangedObjectElementObjectZipper[JObjectZipper[Parent]](obj, this, field)
  }

  def down(field: String)(implicit ev: Self <:< JObjectZipper[_]): JsonZipper[Self] = subZipper(here(field), field)

  def down_?(idx: Int): Option[JsonZipper[Self]] = None
  def down_?(field: String): Option[JsonZipper[Self]] = if(here.contains(field)) Some(down(field)) else None

  def remove(field: String): Self = down_?(field).map(_.remove.up).getOrElse(this)

  def map(f: (String, JsonZipper[JObjectZipper[Parent]]) => ZipperLike[JObjectZipper[Parent]]) = {
    here.fields.foldLeft(this : JObjectZipper[Parent]) { (newSelf, fieldChild) =>
      val (field, child) = fieldChild
      f(field, newSelf.subZipper(child, field)).up
    }
  }

  def first: JsonZipper[Self] = down(fields.first)
  def last: JsonZipper[Self] = down(fields.last)

  def first_? : Option[JsonZipper[Self]] = if(!here.isEmpty) Some(first) else None
  def last_? : Option[JsonZipper[Self]] = if(!here.isEmpty) Some(last) else None

  def contains(f: String) = fields.contains(f)

  protected lazy val fields = {
    val f = new java.util.TreeSet[String]
    here.fields.keySet.foreach(f.add)
    f
  }

  def leastAbove(f: String): Option[String] = {
    val it = fields.tailSet(f).iterator // tailset == all those >= f
    if(it.hasNext) {
      val first = it.next()
      if(first == f) {
        if(it.hasNext) Some(it.next())
        else None
      } else {
        Some(first)
      }
    } else {
      None
    }
  }

  def greatestBelow(f: String): Option[String] = {
    val heads = fields.headSet(f) // headset == all those < f
    if(heads.isEmpty) None
    else Some(heads.last)
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

  private[zipper] val stringComparator = new java.util.Comparator[String] {
    def compare(a: String, b: String) = a.compareTo(b)
  }

  private[zipper] def badUserNoCookie() = throw new Exception("Falsifying evidence?  You would make a good cop!")
}

// TOP LEVEL

sealed abstract trait Root // phantom

private[zipper] class TopLevelZipperLike { this: ZipperLike[Root] =>
  def up(implicit ev: Root <:< JsonZipper[_]) = JsonZipper.badUserNoCookie()
  def up_? = None

  def next(implicit ev: Root <:< JsonZipper[_]): Option[JsonZipper[Root]] = JsonZipper.badUserNoCookie()
  def prev(implicit ev: Root <:< JsonZipper[_]): Option[JsonZipper[Root]] = JsonZipper.badUserNoCookie()

  def next_? : Option[JsonZipper[Root]] = None
  def prev_? : Option[JsonZipper[Root]] = None

  def position(implicit ev: Root <:< JArrayZipper[_]): Int = JsonZipper.badUserNoCookie()
  def field(implicit ev: Root <:< JObjectZipper[_]): String = JsonZipper.badUserNoCookie()

  def position_? : Option[Int] = None
  def field_? : Option[String] = None

  def replace(newValue: JAtom) = new TopLevelAtomZipper(newValue)
  def replace(newValue: JArray) = new TopLevelArrayZipper(newValue)
  def replace(newValue: JObject) = new TopLevelObjectZipper(newValue)
}

private[zipper] object TopLevelNothingZipper extends TopLevelZipperLike with NothingZipper[Root] {
  def top(implicit ev: Root <:< JsonZipper[_]) = JsonZipper.badUserNoCookie()
  def top_? = None
}

private[zipper] class TopLevelZipper extends TopLevelZipperLike { this: JsonZipper[Root] =>
  def remove = TopLevelNothingZipper
  def top: JsonZipper[_] = this
}

private[zipper] class TopLevelAtomZipper(val here: JAtom) extends TopLevelZipper with JAtomZipper[Root]
private[zipper] class TopLevelArrayZipper(val here: JArray) extends TopLevelZipper with JArrayZipper[Root]
private[zipper] class TopLevelObjectZipper(val here: JObject) extends TopLevelZipper with JObjectZipper[Root]

// CODE COMMON TO ALL "ELEMENT" ZIPPER(LIKE)S

private[zipper] abstract class ElementZipperLike[Parent <: JCompoundZipper[_]](val parent: Parent) { this: ZipperLike[Parent] =>
  def up(implicit ev: Parent <:< JsonZipper[_]) = parent
  def up_? = Some(up)

  def next_? = next
  def prev_? = prev
}

private[zipper] trait ElementZipper[Parent <: JCompoundZipper[_]] { this: ElementZipperLike[Parent] with JsonZipper[Parent] =>
  def top = {
    def loop(last: JsonZipper[_], next: Option[JsonZipper[_]]): JsonZipper[_] = {
      next match {
        case None => last
        case Some(p) => loop(p, p.up_?)
      }
    }
    loop(this, up_?).asInstanceOf[JsonZipper[Root]]
  }
}

private[zipper] trait ElementNothingZipper[Parent <: JCompoundZipper[_]] { this: ElementZipperLike[Parent] with NothingZipper[Parent] =>
  def top(implicit ev: Parent <:< JsonZipper[_]): JsonZipper[_] = up.top
  def top_? = Some(up.top)
}

// ARRAY ELEMENT, CHANGED OR NOT

private[zipper] abstract class ArrayElementZipperLike[Parent <: JArrayZipper[_]](p: Parent, val idxInParent: Int) extends ElementZipperLike(p) { this: ZipperLike[Parent] =>
  def replace(newValue: JAtom) = new ChangedArrayElementAtomZipper(newValue, parent, idxInParent)
  def replace(newValue: JArray) = new ChangedArrayElementArrayZipper(newValue, parent, idxInParent)
  def replace(newValue: JObject) = new ChangedArrayElementObjectZipper(newValue, parent, idxInParent)

  def remove = new ChangedArrayElementNothingZipper(parent, idxInParent)

  def next(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]] = {
    up.down_?(idxInParent + 1).asInstanceOf[Option[JsonZipper[Parent]]]
  }

  def prev(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]] = {
    up.down_?(idxInParent - 1).asInstanceOf[Option[JsonZipper[Parent]]]
  }

  def position(implicit ev: Parent <:< JArrayZipper[_]): Int = idxInParent
  def field(implicit ev: Parent <:< JObjectZipper[_]): String = JsonZipper.badUserNoCookie()

  def position_? : Option[Int] = Some(position)
  def field_? : Option[String] = None
}

private[zipper] abstract class ArrayElementZipper[Parent <: JArrayZipper[_]](p: Parent, i: Int) extends ArrayElementZipperLike(p, i) with ElementZipper[Parent] { this: JsonZipper[Parent] =>
}

private[zipper] trait ArrayElementArrayZipper[Parent <: JArrayZipper[_]] { this: ArrayElementZipper[Parent] with JArrayZipper[Parent] =>
}

private[zipper] trait ArrayElementObjectZipper[Parent <: JArrayZipper[_]] { this: ArrayElementZipper[Parent] with JObjectZipper[Parent] =>
}

// OBJECT ELEMENT, CHANGED OR NOT


private[zipper] abstract class ObjectElementZipperLike[Parent <: JObjectZipper[_]](p: Parent, val fieldInParent: String) extends ElementZipperLike(p) { this: ZipperLike[Parent] =>
  def replace(newValue: JAtom) = new ChangedObjectElementAtomZipper(newValue, parent, fieldInParent)
  def replace(newValue: JArray) = new ChangedObjectElementArrayZipper(newValue, parent, fieldInParent)
  def replace(newValue: JObject) = new ChangedObjectElementObjectZipper(newValue, parent, fieldInParent)

  def remove = new ChangedObjectElementNothingZipper(parent, fieldInParent)

  def next(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]] = {
    val p = up
    p.leastAbove(fieldInParent) match {
      case Some(nextField) => Some(p.down(nextField).asInstanceOf[JsonZipper[Parent]])
      case None => None
    }
  }

  def prev(implicit ev: Parent <:< JsonZipper[_]): Option[JsonZipper[Parent]] = {
    val p = up
    p.greatestBelow(fieldInParent) match {
      case Some(nextField) => Some(p.down(nextField).asInstanceOf[JsonZipper[Parent]])
      case None => None
    }
  }

  def position(implicit ev: Parent <:< JArrayZipper[_]): Int = JsonZipper.badUserNoCookie()
  def field(implicit ev: Parent <:< JObjectZipper[_]): String = fieldInParent

  def position_? : Option[Int] = None
  def field_? : Option[String] = Some(field)
}

private[zipper] abstract class ObjectElementZipper[Parent <: JObjectZipper[_]](p: Parent, f: String) extends ObjectElementZipperLike(p, f) with ElementZipper[Parent] { this: JsonZipper[Parent] =>
}

private[zipper] trait ObjectElementArrayZipper[Parent <: JObjectZipper[_]] { this: ObjectElementZipper[Parent] with JArrayZipper[Parent] =>
}

private[zipper] trait ObjectElementObjectZipper[Parent <: JObjectZipper[_]] { this: ObjectElementZipper[Parent] with JObjectZipper[Parent] =>
}

// ARRAY ELEMENT BUT UNCHANGED

private[zipper] abstract class UnchangedArrayElementZipper[Parent <: JArrayZipper[_]](p: Parent, i: Int) extends ArrayElementZipper[Parent](p, i) { this: JsonZipper[Parent] =>
}

private[zipper] class UnchangedArrayElementAtomZipper[Parent <: JArrayZipper[_]](val here: JAtom, p: Parent, i: Int) extends UnchangedArrayElementZipper[Parent](p, i) with JAtomZipper[Parent]
private[zipper] class UnchangedArrayElementArrayZipper[Parent <: JArrayZipper[_]](val here: JArray, p: Parent, i: Int) extends UnchangedArrayElementZipper[Parent](p, i) with ArrayElementArrayZipper[Parent] with JArrayZipper[Parent]
private[zipper] class UnchangedArrayElementObjectZipper[Parent <: JArrayZipper[_]](val here: JObject, p: Parent, i: Int) extends UnchangedArrayElementZipper[Parent](p, i) with ArrayElementObjectZipper[Parent] with JObjectZipper[Parent]

// OBJECT ELEMENT BUT UNCHANGED

private[zipper] abstract class UnchangedObjectElementZipper[Parent <: JObjectZipper[_]](p: Parent, f: String) extends ObjectElementZipper(p, f) { this: JsonZipper[Parent] =>
}

private[zipper] class UnchangedObjectElementAtomZipper[Parent <: JObjectZipper[_]](val here: JAtom, p: Parent, f: String) extends UnchangedObjectElementZipper(p, f) with JAtomZipper[Parent]
private[zipper] class UnchangedObjectElementArrayZipper[Parent <: JObjectZipper[_]](val here: JArray, p: Parent, f: String) extends UnchangedObjectElementZipper(p, f) with ObjectElementArrayZipper[Parent] with JArrayZipper[Parent]
private[zipper] class UnchangedObjectElementObjectZipper[Parent <: JObjectZipper[_]](val here: JObject, p: Parent, f: String) extends UnchangedObjectElementZipper(p, f) with ObjectElementObjectZipper[Parent] with JObjectZipper[Parent]

// ARRAY ELEMENT AND CHANGED

private[zipper] abstract class ChangedArrayElementZipper[Parent <: JArrayZipper[_]](p: Parent, i: Int) extends ArrayElementZipper(p, i) { this: JsonZipper[Parent] =>
  override def up(implicit ev: Parent <:< JsonZipper[_]) = parent.replace(JArray(parent.here.toSeq.updated(idxInParent, here))).asInstanceOf[Parent]
}

private[zipper] class ChangedArrayElementAtomZipper[Parent <: JArrayZipper[_]](val here: JAtom, p: Parent, i: Int) extends ChangedArrayElementZipper(p, i) with JAtomZipper[Parent]
private[zipper] class ChangedArrayElementArrayZipper[Parent <: JArrayZipper[_]](val here: JArray, p: Parent, i: Int) extends ChangedArrayElementZipper(p, i) with ArrayElementArrayZipper[Parent] with JArrayZipper[Parent]
private[zipper] class ChangedArrayElementObjectZipper[Parent <: JArrayZipper[_]](val here: JObject, p: Parent, i: Int) extends ChangedArrayElementZipper(p, i) with ArrayElementObjectZipper[Parent] with JObjectZipper[Parent]

private[zipper] class ChangedArrayElementNothingZipper[Parent <: JArrayZipper[_]](p: Parent, i: Int) extends ArrayElementZipperLike(p, i) with ElementNothingZipper[Parent] with NothingZipper[Parent] {
  override def up(implicit ev: Parent <:< JsonZipper[_]) = parent.replace(JArray(parent.here.toSeq.patch(idxInParent, Nil, 1))).asInstanceOf[Parent]
}

// OBJECT ELEMENT AND CHANGED

private[zipper] abstract class ChangedObjectElementZipper[Parent <: JObjectZipper[_]](p: Parent, f: String) extends ObjectElementZipper(p, f) with ElementZipper[Parent] { this: JsonZipper[Parent] =>
  override def up(implicit ev: Parent <:< JsonZipper[_]) = parent.replace(JObject(parent.here.fields.updated(fieldInParent, here))).asInstanceOf[Parent]
}

private[zipper] class ChangedObjectElementAtomZipper[Parent <: JObjectZipper[_]](val here: JAtom, p: Parent, f: String) extends ChangedObjectElementZipper(p, f) with JAtomZipper[Parent]
private[zipper] class ChangedObjectElementArrayZipper[Parent <: JObjectZipper[_]](val here: JArray, p: Parent, f: String) extends ChangedObjectElementZipper(p, f) with ObjectElementArrayZipper[Parent] with JArrayZipper[Parent]
private[zipper] class ChangedObjectElementObjectZipper[Parent <: JObjectZipper[_]](val here: JObject, p: Parent, f: String) extends ChangedObjectElementZipper(p, f) with ObjectElementObjectZipper[Parent] with JObjectZipper[Parent]

private[zipper] class ChangedObjectElementNothingZipper[Parent <: JObjectZipper[_]](parent: Parent, field: String) extends ObjectElementZipperLike(parent, field) with ElementNothingZipper[Parent] with NothingZipper[Parent] {
  override def up(implicit ev: Parent <:< JsonZipper[_]) = parent.replace(JObject(parent.here.fields - field)).asInstanceOf[Parent]
}
