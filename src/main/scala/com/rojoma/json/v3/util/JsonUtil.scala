package com.rojoma.json.v3
package util

import java.io.{Reader, InputStreamReader, FileInputStream, FileOutputStream, OutputStreamWriter, Writer, FilterWriter, BufferedWriter, IOException, File, OutputStream, FilterOutputStream, InputStream, BufferedInputStream, BufferedOutputStream, FilterInputStream}
import java.nio.charset.Charset
import scala.io.Codec

import io._
import codec._

object JsonUtil {
  private class CloseBlockingWriter(writer: Writer) extends FilterWriter(writer) {
    override def close(): Unit = {}
    override def flush(): Unit = {}
  }

  private class CloseBlockingOutputStream(stream: OutputStream) extends FilterOutputStream(stream) {
    override def close(): Unit = {}
    override def flush(): Unit = {}

    // FilterOutputStream overrides bulk-write to use single-byte write
    override def write(buf: Array[Byte]) = out.write(buf)
    override def write(buf: Array[Byte], off: Int, len: Int) = out.write(buf, off, len)
  }

  private class CloseBlockingInputStream(stream: InputStream) extends FilterInputStream(stream) {
    override def close(): Unit = {}
  }

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
  def readJsonBytes[T : JsonDecode](stream: InputStream, charset: Charset, buffer: Boolean = true): Either[DecodeError, T] = {
    val barrier = new CloseBlockingInputStream(stream)
    val buffered = if(buffer) new BufferedInputStream(barrier) else barrier
    readJson[T](new InputStreamReader(buffered, charset), buffer = buffer)
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

  @deprecated(message = "Provide a Codec or Charset", since = "3.5.0")
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

  @deprecated(message = "Provide a Codec or Charset", since = "3.5.0")
  @throws(classOf[IOException])
  @throws(classOf[JsonParseException])
  def readJsonFile[T : JsonDecode](filename: File): Either[DecodeError, T] = readJsonFile[T](filename, Codec.default.charSet)

  @throws(classOf[JsonParseException])
  def parseJson[T : JsonDecode](string: String): Either[DecodeError, T] = JsonDecode.fromJValue[T](JsonReader.fromString(string))

  @throws(classOf[IOException])
  def writeJson[T : JsonEncode](writer: Writer, jsonable: T, pretty: Boolean = false, buffer: Boolean = true): Unit = {
    val json = JsonEncode.toJValue(jsonable)

    def write(finalWriter: Writer): Unit = {
      if(pretty) PrettyJsonWriter.toWriter(finalWriter, json)
      else CompactJsonWriter.toWriter(finalWriter, json)
    }

    if(buffer) {
      val barrier = new CloseBlockingWriter(writer)
      val buffer = new BufferedWriter(barrier)
      write(buffer)
      buffer.flush()
    } else {
      write(writer)
    }
  }

  @throws(classOf[IOException])
  def writeJsonBytes[T : JsonEncode](stream: OutputStream, charset: Charset, jsonable: T, pretty: Boolean = false, buffer: Boolean = true): Unit = {
    val barrier = new CloseBlockingOutputStream(stream)
    val buffered = if(buffer) new BufferedOutputStream(barrier) else barrier
    val writer = new OutputStreamWriter(buffered, charset)
    writeJson(writer, jsonable, pretty = pretty, buffer = false)
    writer.flush()
  }

  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: String, charset: Charset, jsonable: T, pretty: Boolean): Unit = {
    val stream = new FileOutputStream(filename)
    try {
      val writer = new OutputStreamWriter(stream, charset)
      try {
        writeJson(writer, jsonable, buffer = true, pretty = pretty)
      } finally {
        writer.close()
      }
    } finally {
      stream.close()
    }
  }
  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: String, charset: Charset, jsonable: T): Unit =
    writeJsonFile(filename, charset, jsonable, false)

  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: String, codec: Codec, jsonable: T, pretty: Boolean): Unit =
    writeJsonFile(filename, codec.charSet, jsonable, pretty = pretty)
  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: String, codec: Codec, jsonable: T): Unit =
    writeJsonFile(filename, codec, jsonable, false)

  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: File, charset: Charset, jsonable: T, pretty: Boolean): Unit = {
    val stream = new FileOutputStream(filename)
    try {
      val writer = new OutputStreamWriter(stream, charset)
      try {
        writeJson(writer, jsonable, buffer = true, pretty = pretty)
      } finally {
        writer.close()
      }
    } finally {
      stream.close()
    }
  }
  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: File, charset: Charset, jsonable: T): Unit =
    writeJsonFile(filename, charset, jsonable, false)

  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: File, codec: Codec, jsonable: T, pretty: Boolean): Unit =
    writeJsonFile(filename, codec.charSet, jsonable, pretty = pretty)
  @throws(classOf[IOException])
  def writeJsonFile[T : JsonEncode](filename: File, codec: Codec, jsonable: T): Unit =
    writeJsonFile(filename, codec, jsonable, false)

  def renderJson[T : JsonEncode](jsonable: T, pretty: Boolean = false) = {
    val json = JsonEncode.toJValue(jsonable)
    if(pretty) PrettyJsonWriter.toString(json)
    else CompactJsonWriter.toString(json)
  }
}

