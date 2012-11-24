package com.rojoma.json

package object io {
  /** A function for use in caching objects' field names, for
    * memory reduction.  In most places, this defaults to
    * `identity`.  In [[com.rojoma.json.io.JsonReader]] however,
    * a [[com.rojoma.json.io.HashMapFieldCache]] is used because
    * it is already known that the entire datum will be read into
    * memory at once. */
  type FieldCache = String => String

  val IdentityFieldCache: FieldCache = identity
}
