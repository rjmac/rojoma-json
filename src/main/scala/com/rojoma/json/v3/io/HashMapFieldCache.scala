package com.rojoma.json.v3
package io

class HashMapFieldCache(maximumDepth: Int) extends FieldCache {
  val cache = new java.util.HashMap[String, String]

  def apply(s: String, depth: Int) = {
    val cached = cache.get(s)
    if(cached == null && depth <= maximumDepth) {
      cache.put(s, s)
      s
    } else {
      cached
    }
  }
}
