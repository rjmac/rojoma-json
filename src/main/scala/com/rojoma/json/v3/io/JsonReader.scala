package com.rojoma.json.v3
package io

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

import java.io.Reader

import ast._

/** Parses a token-stream into a [[com.rojoma.json.v3.ast.JValue]].  As an extension,
  * this parser accepts unquoted strings (i.e., identifiers) as field-names. */
class JsonReader(input: Iterator[JsonEvent]) {
  def this(tokens: Iterator[JsonToken], fieldCache: FieldCache) = this(new JsonEventIterator(tokens, fieldCache))
  def this(reader: Reader, fieldCache: FieldCache) = this(new FusedBlockJsonEventIterator(reader, fieldCache))
  def this(text: String, fieldCache: FieldCache) = this(new java.io.StringReader(text), fieldCache)

  def this(tokens: Iterator[JsonToken])(implicit dummy: com.rojoma.json.v3.`-impl`.StupidErasure = null) = this(tokens, IdentityFieldCache)
  def this(reader: Reader) = this(reader, IdentityFieldCache)
  def this(text: String) = this(text, IdentityFieldCache)

  val lexer = input.buffered

  /** Read one JSON datum out of the input.  If the input consists of more than one
    * object, the input after this call is positioned such that the next item available
    * is the first token after the produced object.
    * @return The [[com.rojoma.json.v3.ast.JValue]] read.
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs. */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def read(): JValue = try {
    lexer.next() match {
      case StartOfObjectEvent() => readObject()
      case StartOfArrayEvent() => readArray()
      case StringEvent(s) => JString(s)
      case NumberEvent(n) => JNumber.unsafeFromString(n)
      case IdentifierEvent("true") => JBoolean.canonicalTrue
      case IdentifierEvent("false") => JBoolean.canonicalFalse
      case IdentifierEvent("null") => JNull
      case i@IdentifierEvent(_) => throw new JsonUnknownIdentifier(i)
      case e@(EndOfObjectEvent() | EndOfArrayEvent() | FieldEvent(_)) =>
        throw new JsonBadParse(e)
    }
  } catch {
    case e: NoSuchTokenException =>
      throw new JsonParserEOF(e.position)
    case _: NoSuchElementException =>
      throw new JsonParserEOF(Position(-1, -1)) // this won't happen with the standard tokenizer and eventer
  }

  private def hopeFor(event: JsonEvent): Boolean = {
    if(event == lexer.head) {
      lexer.next()
      true
    } else {
      false
    }
  }

  private def atEndOfObject = hopeFor(JsonReader.EoOEvent)
  private def readObject(): JObject = {
    if(atEndOfObject) return JObject.canonicalEmpty

    // It's bad practice to rely on this, but we'll preserve the order
    // of elements as they're read (barring duplication).
    val result = new mutable.LinkedHashMap[String, JValue]
    do {
      lexer.next() match {
        case FieldEvent(field) =>
          val value = read()
          result += field -> value
        case event =>
          throw new JsonBadParse(event)
      }
    } while(!atEndOfObject)
    JObject(result)
  }

  private def atEndOfArray = hopeFor(JsonReader.EoAEvent)
  private def readArray(): JArray = {
    if(atEndOfArray) return JArray.canonicalEmpty
    val builder = new VectorBuilder[JValue]
    do {
      builder += read()
    } while(!atEndOfArray)
    JArray(builder.result())
  }
}

object JsonReader {
  def apply(r: Reader, buffer: Boolean = true): JsonReader =
    if(buffer) new JsonReader(new FusedBlockJsonEventIterator(r))
    else new JsonReader(new JsonTokenIterator(r))
  def apply(s: String): JsonReader = new JsonReader(s)

  /** Read a [[com.rojoma.json.v3.ast.JValue]] out of a `Reader`.
    * @param r The source of characters.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @throws `IOException` if a low-level IO error occurs.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  @throws(classOf[java.io.IOException])
  def fromReader(r: Reader, buffer: Boolean = false) =
    apply(r, buffer).read()

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
  def fromTokens(it: Iterator[JsonToken]) = new JsonReader(it).read()

  /** Reads a single datum out of an iterator of events.
    * @param it The source of tokens.  After this call succeeds, the
    * iterator is positioned at the next event after the read datum.
    * @return A [[com.rojoma.json.v3.ast.JValue]]
    * @throws [[com.rojoma.json.v3.io.JsonReaderException]] if a complete object cannot be read.
    * @see [[com.rojoma.json.v3.io.JsonReader]] */
  @throws(classOf[JsonReaderException])
  def fromEvents(it: Iterator[JsonEvent]) = new JsonReader(it).read()

  private val EoAEvent = EndOfArrayEvent()(Position.Invalid)
  private val EoOEvent = EndOfObjectEvent()(Position.Invalid)
}
