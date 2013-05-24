package com.rojoma.json
package util

import java.io.{Reader, BufferedReader, InputStreamReader, FileInputStream, Writer, FilterWriter, BufferedWriter, IOException, File}
import scala.io.Codec

import ast._
import io._
import codec._

object JsonUtil {
  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJson[T : JsonCodec](reader: Reader, buffer: Boolean = false) = {
    val jvalue =
      if(buffer) JsonReader.fromEvents(new FusedBlockJsonEventIterator(reader))
      else JsonReader.fromReader(reader)
    JsonCodec.fromJValue[T](jvalue)
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonCodec](filename: String, codec: Codec): Option[T] = {
    val stream = new FileInputStream(filename)
    try {
      readJson[T](new InputStreamReader(stream, codec.charSet), buffer = true)
    } finally {
      stream.close()
    }
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonCodec](filename: String): Option[T] = readJsonFile[T](filename, Codec.default)

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonCodec](filename: File, codec: Codec): Option[T] = {
    val stream = new FileInputStream(filename)
    try {
      readJson[T](new InputStreamReader(stream, codec.charSet), buffer = true)
    } finally {
      stream.close()
    }
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonCodec](filename: File): Option[T] = readJsonFile[T](filename, Codec.default)

  @throws(classOf[JsonParseException])
  def parseJson[T : JsonCodec](string: String) = JsonCodec.fromJValue[T](JsonReader.fromString(string))

  @throws(classOf[IOException])
  def writeJson[T : JsonCodec](writer: Writer, jsonable: T, pretty: Boolean = false, buffer: Boolean = false) = {
    val json = JsonCodec.toJValue(jsonable)

    def write(finalWriter: Writer) {
      if(pretty) PrettyJsonWriter.toWriter(finalWriter, json)
      else CompactJsonWriter.toWriter(finalWriter, json)
    }

    if(buffer) {
      val barrier = new FilterWriter(writer) {
        override def close() {}
        override def flush() {}
      }
      val buffer = new BufferedWriter(barrier)
      write(buffer)
      buffer.flush()
    } else {
      write(writer)
    }
  }

  def renderJson[T : JsonCodec](jsonable: T, pretty: Boolean = false) = {
    val json = JsonCodec.toJValue(jsonable)
    if(pretty) PrettyJsonWriter.toString(json)
    else CompactJsonWriter.toString(json)
  }
}

