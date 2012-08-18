package com.rojoma.json
package util

import ast._
import codec._

class SimpleHierarchyCodecBuilder[Root <: AnyRef] private[util] (tagType: TagType, subcodecs: Map[String, JsonCodec[_ <: Root]], classes: Map[Class[_], String]) {
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

  def build: JsonCodec[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    tagType match {
      case TagToValue =>
        new JsonCodec[Root] {
          def encode(x: Root): JValue = {
            val (name, subcodec) = codecFor(x)
            JObject(Map(name -> subcodec.asInstanceOf[JsonCodec[Root]].encode(x)))
          }
          def decode(x: JValue): Option[Root] = x match {
            case JObject(fields) =>
              // this should almost always pick the first branch
              if(fields.size <= subcodecs.size) {
                for {
                  (possibleTag, possibleObject) <- fields
                  subcodec <- subcodecs.get(possibleTag)
                  value <- subcodec.decode(possibleObject)
                } return Some(value)
              } else {
                for {
                  (name, subcodec) <- subcodecs
                  field <- fields.get(name)
                  value <- subcodec.decode(field)
                } return Some(value)
              }
              None
            case _ =>
              None
          }
        }
      case TagAndValue(typeField, valueField) =>
        new JsonCodec[Root] {
          def encode(x: Root): JValue = {
            val (name, subcodec) = codecFor(x)
            JObject(Map(typeField -> JString(name),
                        valueField -> subcodec.asInstanceOf[JsonCodec[Root]].encode(x)))
          }
          def decode(x: JValue): Option[Root] = x match {
            case JObject(fields) =>
              for {
                jname <- fields.get(typeField).flatMap(_.cast[JString])
                subcodec <- subcodecs.get(jname.string)
                jvalue <- fields.get(valueField)
                value <- subcodec.decode(jvalue)
              } yield value
            case _ =>
              None
          }
        }
      case InternalTag(typeField, removeForSubcodec) =>
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
                result <- codec.decode(if(removeForSubcodec) JObject(fields - name) else x)
              } yield result
            case _ =>
              None
          }
        }
    }
  }
}

class NoTagSimpleHierarchyCodecBuilder[Root <: AnyRef] private[util] (subcodecs: Seq[(Class[_], JsonCodec[_ <: Root])]) {
  def branch[T <: Root](implicit codec: JsonCodec[T], mfst: ClassManifest[T]) = {
    if(subcodecs.find(_._1 == mfst.erasure).isDefined) throw new IllegalArgumentException("Already defined a codec for class " + mfst.erasure)
    new NoTagSimpleHierarchyCodecBuilder[Root](subcodecs :+ (mfst.erasure -> codec))
  }

  def build: JsonCodec[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    new JsonCodec[Root] {
      val codecsMap = subcodecs.toMap

      private def codecFor(x: Root) =
        codecsMap.get(x.getClass) match {
          case Some(subcodec) => subcodec
          case None => throw new IllegalArgumentException("No codec defined for " + x.getClass)
        }

      def encode(x: Root): JValue = {
        codecFor(x).asInstanceOf[JsonCodec[Root]].encode(x)
      }

      def decode(x: JValue): Option[Root] ={
        for {
          (_, subcodec) <- subcodecs
          value <- subcodec.decode(x)
        } return Some(value)
        None
      }
    }
  }
}

sealed abstract class TagType
case class InternalTag(fieldName: String, removeTagForSubcodec: Boolean = true) extends TagType
case object TagToValue extends TagType
case class TagAndValue(typeField: String, valueField: String) extends TagType {
  if(typeField == valueField) throw new IllegalArgumentException("type field and value field must be different")
}
sealed abstract class NoTag
case object NoTag extends NoTag

object SimpleHierarchyCodecBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyCodecBuilder[Root](tagType, Map.empty, Map.empty)
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyCodecBuilder[Root](Vector.empty)
}
