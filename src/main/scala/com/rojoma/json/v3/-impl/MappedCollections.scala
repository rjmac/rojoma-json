package com.rojoma.json.v3.`-impl`

import scala.{collection => sc}

final class MappedViewSeq[A, +B](underlying: sc.Seq[A], f: A => B) extends sc.Seq[B] {
  override def iterator = underlying.iterator.map(f)
  override def apply(idx: Int) = f(underlying(idx))
  override def length = underlying.length
  override def foreach[U](g: B => U) = underlying.foreach(g compose f)
  override def map[C](g: B => C) = new MappedViewSeq(underlying, g compose f)

  def force = underlying.view.map(f).to(Vector)
}

final class MappedViewMap[A, B, C](underlying: sc.Map[A, B], f: B => C) extends sc.Map[A, C] {
  override def iterator = underlying.iterator.map { case (a, b) => (a, f(b)) }
  @deprecated("Use -- or removeAll on an immutable Map", "2.13.0")
  override def -(key1: A, key2: A, keys: A*) = new MappedViewMap(underlying.-(key1, key2, keys : _*), f)
  @deprecated("Use - or remove on an immutable Map", "2.13.0")
  override def -(key: A) = new MappedViewMap(underlying - key, f)
  override def get(key: A) = underlying.get(key).map(f)
  override def foreach[U](g: ((A, C)) => U) = underlying.foreach { case (k, v) => g(k -> f(v)) }

  def force = underlying.view.mapValues(f).to(sc.Map)
}
