package com.rojoma.json.v3
package io

/** A cache of keys that are known ahead of time. */
class FixedSetFieldCache(keys: TraversableOnce[String]) extends FieldCache {
  private val cache = new java.util.HashMap[String, String]
  for(key <- keys) cache.put(key, key)

  def apply(s: String, depth: Int) = {
    val cached = cache.get(s)
    if(cached == null) s else cached
  }
}
