package com.rojoma.json
package util

import java.io.{Reader, BufferedReader, Writer, FilterWriter, BufferedWriter, IOException}

import ast._
import io._
import codec._

object JsonUtil {
  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJson[T : JsonCodec](reader: Reader, buffer: Boolean = false) = {
    val finalReader = if(buffer) new BufferedReader(reader)
                      else reader
    JsonCodec.fromJValue[T](JsonReader.fromReader(finalReader))
  }

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

