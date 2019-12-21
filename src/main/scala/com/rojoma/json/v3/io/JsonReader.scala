package com.rojoma.json.v3
package io

import java.io.{Reader, StringReader}

import ast._

trait JsonReader {
  def read(): JValue
}

object JsonReader {
  def apply(r: Reader, buffer: Boolean = true, blockSize: Int = 1024): JsonReader =
    if(buffer) new FusedBlockJsonReader(r, blockSize = blockSize)
    else new EventJsonReader(new JsonTokenIterator(r))
  def apply(s: String): JsonReader = new FusedBlockJsonReader(s)

  def unbufferedString(s: String): JsonReader= new EventJsonReader(new JsonTokenIterator(new StringReader(s)))
  def semibufferedString(s: String): JsonReader= new EventJsonReader(new FusedBlockJsonEventIterator(s))

  /** Read a [[com.rojoma.json.v3.ast.JValue]] out of a `Reader`.
    * @param r The source of characters.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def fromReader(r: Reader, buffer: Boolean = true, blockSize: Int = 1024) =
    apply(r, buffer, blockSize).read()

  /** Read a [[com.rojoma.json.v3.ast.JValue]] out of a `String`.
    * @param s The source of characters.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  def fromString(s: String) = apply(s).read()

  /** Reads a single datum out of an iterator of tokens.
    * @param it The source of tokens.  After this call succeeds, the
    * iterator is positioned at the next token after the read datum.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  def fromTokens(it: Iterator[JsonToken]) = new EventJsonReader(it).read()

  /** Reads a single datum out of an iterator of events.
    * @param it The source of tokens.  After this call succeeds, the
    * iterator is positioned at the next event after the read datum.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  def fromEvents(it: Iterator[JsonEvent]) = new EventJsonReader(it).read()
}
