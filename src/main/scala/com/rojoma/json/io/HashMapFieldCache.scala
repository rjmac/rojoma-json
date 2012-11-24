package com.rojoma.json
package io

class HashMapFieldCache extends FieldCache {
  val cache = new java.util.HashMap[String, String]

  def apply(s: String) = {
    val cached = cache.get(s)
    if(cached == null) {
      cache.put(s, s)
      s
    } else {
      cached
    }
  }
}

