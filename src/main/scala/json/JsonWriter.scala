package json
package io

import scala.{collection => sc}

import ast._

trait JsonWriter {
  def write(jobject: JValue) {
    jobject match {
      case JArray(elements) =>
        writeArray(elements)
      case JObject(fields) =>
        writeObject(fields)
      case JString(str) =>
        writeString(str)
      case JBoolean(bool) =>
        writeBoolean(bool)
      case JNull =>
        writeNull()
      case JFloatingPoint(dbl) =>
        writeDouble(dbl)
      case JIntegral(i) =>
        writeLong(i)
    }
  }

  protected def writeArray(elements: sc.Seq[JValue])
  protected def writeObject(fields: sc.Map[String, JValue])
  protected def writeString(s: String)
  protected def writeBoolean(b: Boolean)
  protected def writeNull()
  protected def writeDouble(dbl: Double)
  protected def writeLong(l: Long)
}

