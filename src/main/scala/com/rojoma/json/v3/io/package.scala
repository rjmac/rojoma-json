package com.rojoma.json.v3

package object io {
  /** A function for use in caching objects' field names, for
    * memory reduction.  In most places, this defaults to
    * `identity`.  In [[com.rojoma.json.v3.io.JsonReader]] however,
    * a [[com.rojoma.json.v3.io.HashMapFieldCache]] is used because
    * it is already known that the entire datum will be read into
    * memory at once.
    * 
    * The parameters are the field names and the depth of nesting
    * at which it occurs. */
  type FieldCache = (String, Int) => String

  val IdentityFieldCache: FieldCache = (f, _) => f
}
