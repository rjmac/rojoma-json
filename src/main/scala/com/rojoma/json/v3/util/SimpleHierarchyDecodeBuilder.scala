package com.rojoma.json.v3
package util

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
                    case Left(err) => pendingErrors += err.prefix(possibleTag)
                  }
                }
              } else {
                for {
                  (tag, subdec) <- subcodecs
                  field <- fields.get(tag)
                } {
                  subdec.decode(field) match {
                    case r@Right(_) => return r
                    case Left(err) => pendingErrors += err.prefix(tag)
                  }
                }
              }
              val errorsFound = pendingErrors.result()
              if(errorsFound.nonEmpty) Left(DecodeError.join(errorsFound))
              else Left(DecodeError.Multiple(subcodecs.keys.toSeq.map(DecodeError.MissingField(_))))
            case other =>
              Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
          }
        }
      case TagAndValue(typeField, valueField) =>
        new JsonDecode[Root] {
          def decode(x: JValue): Either[DecodeError, Root] = x match {
            case JObject(fields) =>
              fields.get(typeField) match {
                case Some(jTypeTag@JString(typeTag)) =>
                  subcodecs.get(typeTag) match {
                    case Some(subDec) =>
                      fields.get(valueField) match {
                        case Some(jvalue) =>
                          subDec.decode(jvalue) match {
                            case r@Right(_) => r
                            case Left(err) => Left(err.prefix(valueField))
                          }
                        case None =>
                          Left(DecodeError.MissingField(valueField))
                      }
                    case None =>
                      Left(DecodeError.InvalidValue(jTypeTag, Path(typeField)))
                  }
                case Some(other) =>
                  Left(DecodeError.InvalidType(expected = JString, got = other.jsonType, Path(typeField)))
                case None =>
                  Left(DecodeError.MissingField(typeField))
              }
            case other =>
              Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType, Path.empty))
          }
        }
      case InternalTag(typeField, removeForSubcodec) =>
        new JsonDecode[Root] {
          def decode(x: JValue): Either[DecodeError, Root] = x match {
            case JObject(fields) =>
              fields.get(typeField) match {
                case Some(jTypeTag@JString(typeTag)) =>
                  subcodecs.get(typeTag) match {
                    case Some(subDec) =>
                      subDec.decode(if(removeForSubcodec) JObject(fields.view.filter(_._1 != typeField).toMap) else x) // no need to augment error results since we're not moving downward
                    case None =>
                      Left(DecodeError.InvalidValue(jTypeTag, Path(typeField)))
                  }
                case Some(other) =>
                  Left(DecodeError.InvalidType(expected = JString, got = other.jsonType, Path(typeField)))
                case None =>
                  Left(DecodeError.MissingField(typeField))
              }
            case other =>
              Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
          }
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
    }
  }
}

object SimpleHierarchyDecodeBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyDecodeBuilder[Root](tagType, Map.empty, ClassAwareMap.empty)
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyDecodeBuilder[Root](Vector.empty)
}
