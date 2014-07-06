package com.rojoma.json.v3
package `-impl`.util

// A simple immutable map for classes which takes into account variance
// (i.e., a map containing a classOf[Object] will find the associated
// value when interrogated for a classOf[String]).  More-specific classes
// override less-specific ones; for equally-inspecific classes, later-
// added wins.
//
// Note: when calling get, hits are O(1) [expected] and misses are O(n)
class ClassAwareMap[+V] private (map: Map[Class[_], V], order: Vector[Class[_]]) {
  def this() = this(Map.empty, Vector.empty)

  // cache for looking up subclasses of explicitly-added items
  private[this] val keyedMap = new java.util.concurrent.ConcurrentHashMap[Class[_], V]

  private def augmentWith(c: Class[_]): Option[V] = synchronized {
    keyedMap.get(c) match {
      case null =>
        val it = order.iterator
        var best: Class[_] = null
        while(it.hasNext) {
          val existing = it.next()
          if(existing.isAssignableFrom(c)) {
            if(best == null) best = existing
            else if(best.isAssignableFrom(existing)) best = existing
          }
        }
        if(best == null) {
          None
        } else {
          val v = map(best)
          keyedMap.put(best, v)
          Some(v)
        }
      case v =>
        Some(v)
    }
  }

  def get(c: Class[_]): Option[V] =
    keyedMap.get(c) match {
      case null =>
        augmentWith(c)
      case v =>
        Some(v)
    }

  def +[V2 >: V](cv: (Class[_], V2)) = new ClassAwareMap[V2](map + cv, order :+ cv._1)

  def containsExact(c: Class[_]) = map.contains(c)
}

object ClassAwareMap {
  private[this] val Empty = new ClassAwareMap[Nothing]
  def empty[T]: ClassAwareMap[T] = Empty
}
