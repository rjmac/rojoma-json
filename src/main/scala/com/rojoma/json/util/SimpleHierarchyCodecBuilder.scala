package com.rojoma.json
package util

import ast._
import codec._
import SimpleHierarchyCodecBuilder._

class SimpleHierarchyCodecBuilder[Root] private (tagType: TagType, subcodecs: Map[String, JsonCodec[_ <: Root]], classes: Map[Class[_], String]) {
  def branch[T <: Root](name: String)(implicit codec: JsonCodec[T], mfst: ClassManifest[T]) = {
    if(subcodecs contains name) throw new IllegalArgumentException("Already defined a codec for branch " + name)
    if(classes contains mfst.erasure) throw new IllegalArgumentException("Already defined a codec for class " + mfst.erasure)
    new SimpleHierarchyCodecBuilder[Root](tagType, subcodecs + (name -> codec), classes + (mfst.erasure -> name))
  }

  private def codecFor(x: Root) =
    classes.get(x.getClass) match {
      case Some(name) => (name, subcodecs(name))
      case None => throw new IllegalArgumentException("No codec defined for " + x.getClass)
    }

  def gen: JsonCodec[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    tagType match {
      case External =>
        new JsonCodec[Root] {
          def encode(x: Root): JValue = {
            val (name, subcodec) = codecFor(x)
            JObject(Map(name -> subcodec.asInstanceOf[JsonCodec[Root]].encode(x)))
          }
          def decode(x: JValue): Option[Root] = x match {
            case JObject(fields) =>
              for {
                (name, subcodec) <- subcodecs
                field <- fields.get(name)
                value <- subcodec.decode(field)
              } return Some(value)
              None
            case _ =>
              None
          }
        }
      case Internal(typeField) =>
        new JsonCodec[Root] {
          def encode(x: Root): JValue = {
            val (name, subcodec) = codecFor(x)
            subcodec.asInstanceOf[JsonCodec[Root]].encode(x) match {
              case JObject(fields) =>
                if(fields contains typeField) throw new IllegalArgumentException("Encoded form of value already contains field " + typeField)
                JObject(fields + (typeField -> JString(name)))
              case _ =>
                throw new IllegalArgumentException("Encoded form of value is not a JObject")
            }
          }
          def decode(x: JValue): Option[Root] = x match {
            case JObject(fields) =>
              for {
                jname <- fields.get(typeField).flatMap(_.cast[JString])
                name = jname.string
                codec <- subcodecs.get(name)
                result <- codec.decode(JObject(fields - name))
              } yield result
            case _ =>
              None
          }
        }
    }
  }
}


object SimpleHierarchyCodecBuilder {
  sealed abstract class TagType
  case class Internal(fieldName: String) extends TagType
  case object External extends TagType

  def apply[Root](tagType: TagType) = new SimpleHierarchyCodecBuilder[Root](tagType, Map.empty, Map.empty)
}
