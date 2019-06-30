package com.rojoma.json.v3.`-impl`.util

import scala.collection.IterableOnce

object FlatteningIteratorUtils {
  implicit class Fit[T](val underlying: Iterator[T]) extends AnyVal {
    def **[T2 >: T](that: => IterableOnce[T2]): Iterator[T2] = underlying match {
      case f: FlatteningIterator[T] => f ++ that
      case _ => new FlatteningIterator(underlying, Vector(() => that.iterator))
    }

    def flatify[A](implicit ev: T <:< IterableOnce[A]): Iterator[A] =
      if(!underlying.hasNext) Iterator.empty
      else {
        val hd = underlying.next().iterator
        hd ** underlying.flatify
      }
  }
}

object FlatteningIterator {
  def apply[T](its: Iterator[T]*): Iterator[T] = {
    if(its.isEmpty) Iterator.empty
    else {
      val i = its.iterator
      val first = i.next()
      val rest: Vector[() => Iterator[T]] = i.map { x => () => x }.toVector
      new FlatteningIterator(first, rest)
    }
  }
}

import FlatteningIteratorUtils._

final class FlatteningIterator[+T] private [util] (private[this] var first: Iterator[T], private[this] var xs: Vector[() => IterableOnce[T]]) extends AbstractIterator[T] {
  def this(it: Iterator[T]) = this(it, Vector.empty)

  // accessors for shiftIterators to use
  private def firstIterator = first
  private def subIterators = xs

  // Normalize the first iterator to be a not-FlatteningIterator.  It
  // should never be one -- if it is one, we've lost our "never nest"
  // promise.
  if(first.isInstanceOf[FlatteningIterator[_]]) {
    val f = first.asInstanceOf[FlatteningIterator[T]]
    first = f.firstIterator
    xs = vec_++(f.subIterators, xs)
  }

  // Exists because of SI-4442
  private def vec_++[A](a: Vector[A], b: Vector[A]): Vector[A] = {
    def cons = {
      var res = b
      val it = a.reverseIterator
      while(it.hasNext) {
        res = it.next() +: res
      }
      res
    }

    def snoc =  {
      var res = a
      val it = b.iterator
      while(it.hasNext) {
        res = res :+ it.next()
      }
      res
    }

    if(a.size < b.size) cons
    else snoc
  }

  override def flatMap[B](f: T => IterableOnce[B]): Iterator[B] =
    map(f).flatify

  def hasNext: Boolean = {
    first.nonEmpty || shiftIterators()
  }

  private def shiftIterators(): Boolean = {
    do {
      // This is a little messy because of the requirement to
      // evaluate xs(i) exactly once.

      val count = xs.size
      var toDrop = 0
      var found: Iterator[T] = null
      while(toDrop != count && found == null) {
        val it = xs(toDrop)().iterator
        if(it.hasNext) found = it
        toDrop += 1 // yes, we're still going to drop it even if found
      }

      found match {
        case null =>
          xs = Vector.empty
          return false
        case f: FlatteningIterator[T] =>
          first = f.firstIterator // Not itself a flattening iterator
          xs = vec_++(f.subIterators, xs.drop(toDrop))
        case plainIterator =>
          first = plainIterator
          xs = xs.drop(toDrop)
          return true
      }
    } while(first.isEmpty)
    true
  }

  def next(): T = {
    if(!hasNext) Iterator.empty.next()
    first.next()
  }
}
