package com.rojoma.json.v3
package util

import scala.language.existentials
import scala.reflect.ClassTag

import ast._
import codec._

import com.rojoma.json.v3.`-impl`.util.ClassAwareMap

class SimpleHierarchyDecodeBuilder[Root <: AnyRef] private[util] (tagType: TagType, subcodecs: Map[String, JsonDecode[_ <: Root]], classes: ClassAwareMap[String]) {
  def branch[T <: Root](name: String)(implicit dec: JsonDecode[T], mfst: ClassTag[T]) = {
    val cls = mfst.runtimeClass
    if(subcodecs contains name) throw new IllegalArgumentException("Already defined a decoder for branch " + name)
    if(classes containsExact cls) throw new IllegalArgumentException("Already defined a decoder for class " + cls)
    new SimpleHierarchyDecodeBuilder[Root](tagType, subcodecs + (name -> dec), classes + (cls -> name))
  }

  private def decFor(x: Root) =
    classes.get(x.getClass) match {
      case Some(name) => (name, subcodecs(name))
      case None => throw new IllegalArgumentException("No decoder defined for " + x.getClass)
    }

  def build: JsonDecode[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    tagType match {
      case TagToValue =>
        new JsonDecode[Root] {
          def decode(x: JValue): Either[DecodeError, Root] = x match {
            case JObject(fields) =>
              // this should almost always pick the first branch
              val pendingErrors = List.newBuilder[DecodeError]
              if(fields.size <= subcodecs.size) {
                for {
                  (possibleTag, possibleObject) <- fields
                  subdec <- subcodecs.get(possibleTag)
                } {
                  subdec.decode(possibleObject) match {
                    case r@Right(_) => return r
                    case Left(err) => pendingErrors += err.augment(Path.Field(possibleTag))
                  }
                }
              } else {
                for {
                  (tag, subdec) <- subcodecs
                  field <- fields.get(tag)
                } {
                  subdec.decode(field) match {
                    case r@Right(_) => return r
                    case Left(err) => pendingErrors += err.augment(Path.Field(tag))
                  }
                }
              }
              val errorsFound = pendingErrors.result()
              if(errorsFound.nonEmpty) Left(DecodeError.join(errorsFound))
              else Left(DecodeError.Multiple(subcodecs.keys.toSeq.map(DecodeError.MissingField(_, Path.empty))))
            case other =>
              Left(DecodeError.InvalidType(JObject, other.jsonType, Path.empty))
          }
          def acceptTypes = SimpleHierarchyDecodeBuilder.justJObject
        }
      case TagAndValue(typeField, valueField) =>
        new JsonDecode[Root] {
          def decode(x: JValue): Either[DecodeError, Root] = x match {
            case JObject(fields) =>
              val typeTag = fields.getOrElse(typeField, return Left(DecodeError.MissingField(typeField, Path.empty))) match {
                case JString(s) => s
                case other => return Left(DecodeError.InvalidType(JString, other.jsonType, Path.empty))
              }
              val subDec = subcodecs.getOrElse(typeTag, return Left(DecodeError.InvalidValue(JString(typeTag), Path.empty)))
              val jvalue = fields.getOrElse(valueField, return Left(DecodeError.MissingField(valueField, Path.empty)))
              subDec.decode(jvalue) match {
                case r@Right(_) => r
                case Left(err) => Left(err.augment(Path.Field(valueField)))
              }
            case other =>
              Left(DecodeError.InvalidType(JObject, other.jsonType, Path.empty))
          }
          def acceptTypes = SimpleHierarchyDecodeBuilder.justJObject
        }
      case InternalTag(typeField, removeForSubcodec) =>
        new JsonDecode[Root] {
          def decode(x: JValue): Either[DecodeError, Root] = x match {
            case JObject(fields) =>
              val typeTag = fields.getOrElse(typeField, return Left(DecodeError.MissingField(typeField, Path.empty))) match {
                case JString(s) => s
                case other => return Left(DecodeError.InvalidType(JString, other.jsonType, Path.empty))
              }
              val subDec = subcodecs.getOrElse(typeTag, return Left(DecodeError.InvalidValue(JString(typeTag), Path.empty)))
              subDec.decode(if(removeForSubcodec) JObject(fields-typeField) else x) // no need to augment error results since we're not moving downward
            case other =>
              Left(DecodeError.InvalidType(JObject, other.jsonType, Path.empty))
          }
          def acceptTypes = SimpleHierarchyDecodeBuilder.justJObject
        }
    }
  }
}

class NoTagSimpleHierarchyDecodeBuilder[Root <: AnyRef] private[util] (subcodecs: Seq[(Class[_], JsonDecode[_ <: Root])]) {
  def branch[T <: Root](implicit dec: JsonDecode[T], mfst: ClassTag[T]) = {
    val cls = mfst.runtimeClass
    if(subcodecs.find(_._1 == cls).isDefined) throw new IllegalArgumentException("Already defined a codec for class " + cls)
    new NoTagSimpleHierarchyDecodeBuilder[Root](subcodecs :+ (cls -> dec))
  }

  def build: JsonDecode[Root] = {
    if(subcodecs.isEmpty) throw new IllegalStateException("No branches defined")
    new JsonDecode[Root] {
      def decode(x: JValue): Either[DecodeError, Root] = {
        val accumulatedErrors = List.newBuilder[DecodeError]
        for((_, subDec) <- subcodecs) {
          subDec.decode(x) match {
            case r@Right(_) => return r
            case Left(e) => accumulatedErrors += e
          }
        }
        Left(DecodeError.join(accumulatedErrors.result()))
      }

      lazy val acceptTypes = subcodecs.foldLeft(Set.empty[JsonType]) { (acc, clsDec) =>
        acc ++ clsDec._2.acceptTypes
      }
    }
  }
}

object SimpleHierarchyDecodeBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyDecodeBuilder[Root](tagType, Map.empty, ClassAwareMap.empty)
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyDecodeBuilder[Root](Vector.empty)

  private val justJObject = Set[JsonType](JObject)
}
