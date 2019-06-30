package com.rojoma.json.v3
package util

import scala.language.existentials
import scala.reflect.ClassTag

import ast._
import codec._

import com.rojoma.json.v3.`-impl`.util.ClassAwareMap

class SimpleHierarchyEncodeBuilder[Root <: AnyRef] private[util] (tagType: TagType, subcodecs: Map[String, JsonEncode[_ <: Root]], classes: ClassAwareMap[String]) {
  def branch[T <: Root](name: String)(implicit enc: JsonEncode[T], mfst: ClassTag[T]) = {
    val cls = mfst.runtimeClass
    if(subcodecs contains name) throw new IllegalArgumentException("Already defined a encoder for branch " + name)
    if(classes containsExact cls) throw new IllegalArgumentException("Already defined a encoder for class " + cls)
    new SimpleHierarchyEncodeBuilder[Root](tagType, subcodecs + (name -> enc), classes + (cls -> name))
  }

  private def encFor(x: Root) =
    classes.get(x.getClass) match {
      case Some(name) => (name, subcodecs(name))
      case None => throw new IllegalArgumentException("No encoder defined for " + x.getClass)
    }

  def build: JsonEncode[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    tagType match {
      case TagToValue =>
        new JsonEncode[Root] {
          def encode(x: Root): JValue = {
            val (name, subenc) = encFor(x)
            JObject(Map(name -> subenc.asInstanceOf[JsonEncode[Root]].encode(x)))
          }
        }
      case TagAndValue(typeField, valueField) =>
        new JsonEncode[Root] {
          def encode(x: Root): JValue = {
            val (name, subenc) = encFor(x)
            JObject(Map(typeField -> JString(name),
                        valueField -> subenc.asInstanceOf[JsonEncode[Root]].encode(x)))
          }
        }
      case InternalTag(typeField, removeForSubcodec) =>
        new JsonEncode[Root] {
          def encode(x: Root): JValue = {
            val (name, subenc) = encFor(x)
            subenc.asInstanceOf[JsonEncode[Root]].encode(x) match {
              case JObject(fields) =>
                if(fields contains typeField) throw new IllegalArgumentException("Encoded form of value already contains field " + typeField)
                JObject(fields concat Map(typeField -> JString(name)))
              case _ =>
                throw new IllegalArgumentException("Encoded form of value is not a JObject")
            }
          }
        }
    }
  }
}

class NoTagSimpleHierarchyEncodeBuilder[Root <: AnyRef] private[util] (subcodecs: Seq[(Class[_], JsonEncode[_ <: Root])]) {
  def branch[T <: Root](implicit enc: JsonEncode[T], mfst: ClassTag[T]) = {
    val cls = mfst.runtimeClass
    if(subcodecs.find(_._1 == cls).isDefined) throw new IllegalArgumentException("Already defined a encoder for class " + cls)
    new NoTagSimpleHierarchyEncodeBuilder[Root](subcodecs :+ (cls -> enc))
  }

  def build: JsonEncode[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    new JsonEncode[Root] {
      val encMap = subcodecs.foldLeft(ClassAwareMap.empty[JsonEncode[_ <: Root]])(_ + _)

      private def encFor(x: Root) =
        encMap.get(x.getClass) match {
          case Some(subEnc) => subEnc
          case None => throw new IllegalArgumentException("No encoder defined for " + x.getClass)
        }

      def encode(x: Root): JValue = {
        encFor(x).asInstanceOf[JsonEncode[Root]].encode(x)
      }
    }
  }
}

object SimpleHierarchyEncodeBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyEncodeBuilder[Root](tagType, Map.empty, ClassAwareMap.empty)
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyEncodeBuilder[Root](Vector.empty)
}
