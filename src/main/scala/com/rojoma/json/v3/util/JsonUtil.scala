package com.rojoma.json.v3
package util

import java.io.{Reader, BufferedReader, InputStreamReader, FileInputStream, Writer, FilterWriter, BufferedWriter, IOException, File}
import java.nio.charset.Charset
import scala.io.Codec

import ast._
import io._
import codec._

object JsonUtil {
  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJson[T : JsonDecode](reader: Reader, buffer: Boolean = true): Either[DecodeError, T] = {
    val jvalue =
      if(buffer) JsonReader.fromEvents(new FusedBlockJsonEventIterator(reader))
      else JsonReader.fromTokens(new JsonTokenIterator(reader))
    JsonDecode.fromJValue[T](jvalue)
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: String, charset: Charset): Either[DecodeError, T] = {
    val stream = new FileInputStream(filename)
    try {
      readJson[T](new InputStreamReader(stream, charset), buffer = true)
    } finally {
      stream.close()
    }
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: String, codec: Codec): Either[DecodeError, T] = readJsonFile(filename, codec.charSet)

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: String): Either[DecodeError, T] = readJsonFile[T](filename, Codec.default.charSet)

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: File, charset: Charset): Either[DecodeError, T] = {
    val stream = new FileInputStream(filename)
    try {
      readJson[T](new InputStreamReader(stream, charset), buffer = true)
    } finally {
      stream.close()
    }
  }

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: File, codec: Codec): Either[DecodeError, T] = readJsonFile(filename, codec.charSet)

  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: File): Either[DecodeError, T] = readJsonFile[T](filename, Codec.default.charSet)

  @throws(classOf[JsonParseException])
  def parseJson[T : JsonDecode](string: String): Either[DecodeError, T] = JsonDecode.fromJValue[T](JsonReader.fromString(string))

  @throws(classOf[IOException])
  def writeJson[T : JsonEncode](writer: Writer, jsonable: T, pretty: Boolean = false, buffer: Boolean = true): Unit = {
    val json = JsonEncode.toJValue(jsonable)

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

  def renderJson[T : JsonEncode](jsonable: T, pretty: Boolean = false) = {
    val json = JsonEncode.toJValue(jsonable)
    if(pretty) PrettyJsonWriter.toString(json)
    else CompactJsonWriter.toString(json)
  }
}

