package com.rojoma.json
package util

import ast._
import codec._

class SimpleHierarchyCodecBuilder[Root <: AnyRef] private[util] (tagType: TagType, subcodecs: Map[String, JsonCodec[_ <: Root]], classes: Map[Class[_], String]) {
  private def t(mfst: com.rojoma.`json-impl`.CM[_]) = com.rojoma.`json-impl`.erasureOf(mfst)

  def branch[T <: Root](name: String)(implicit codec: JsonCodec[T], mfst: com.rojoma.`json-impl`.CM[T]) = {
    if(subcodecs contains name) throw new IllegalArgumentException("Already defined a codec for branch " + name)
    if(classes contains t(mfst)) throw new IllegalArgumentException("Already defined a codec for class " + t(mfst))
    new SimpleHierarchyCodecBuilder[Root](tagType, subcodecs + (name -> codec), classes + (t(mfst) -> name))
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
  private def t(mfst: com.rojoma.`json-impl`.CM[_]) = com.rojoma.`json-impl`.erasureOf(mfst)

  def branch[T <: Root](implicit codec: JsonCodec[T], mfst: com.rojoma.`json-impl`.CM[T]) = {
    if(subcodecs.find(_._1 == t(mfst)).isDefined) throw new IllegalArgumentException("Already defined a codec for class " + t(mfst))
    new NoTagSimpleHierarchyCodecBuilder[Root](subcodecs :+ (t(mfst) -> codec))
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

/** Specifies the mechanism for distinguishing among subclasses in a hierarchy with a tag.
 * @see [[com.rojoma.json.util.NoTag]] */
sealed abstract class TagType

/** Specifies that the base codec should add (and possibly remove) an extra field
 * to the objects generated by the subclasses' [[com.rojoma.json.codec.JsonCodec]]s
 * (and they must be objects).
 *
 * @example {{{
 * abstract class Base
 * case class SubclassA(name: String) extends Base
 * case class SubclassB(x: Int, y: Int) extends Base
 *
 * implicit val aCodec = SimpleJsonCodecBuilder[SubclassA].build("name", _.name)
 * implicit val bCodec = SimpleJsonCodecBuilder[SubclassB].build("x", _.x, "y", _.y)
 *
 * val baseCodec = SimpleHierarchyCodecBuilder[Base](InternalTag("type")).
 *    branch[SubclassA]("a").
 *    branch[SubclassB]("b").
 *    build
 *
 * println(baseCodec.encode(SubclassA("John"))) // { "type" : "a", "name" : "John" }
 * println(baseCodec.encode(SubclassB(1, 2))) // { "type" : "b", "x" : 1, "y" : 2 }
 * }}}
 */
case class InternalTag(fieldName: String, removeTagForSubcodec: Boolean = true) extends TagType

/** Specifies that the base codec should wrap the value generated by subclasses'
 * [[com.rojoma.json.codec.JsonCodec]]s in another object containing a single
 * field, which is the tag for that subclass.
 *
 * @example {{{
 * abstract class Base
 * case class SubclassA(name: String) extends Base
 * case class SubclassB(x: Int, y: Int) extends Base
 *
 * implicit val aCodec = SimpleJsonCodecBuilder[SubclassA].build("name", _.name)
 * implicit val bCodec = SimpleJsonCodecBuilder[SubclassB].build("x", _.x, "y", _.y)
 *
 * val baseCodec = SimpleHierarchyCodecBuilder[Base](TagToValue).
 *    branch[SubclassA]("a").
 *    branch[SubclassB]("b").
 *    build
 *
 * println(baseCodec.encode(SubclassA("John"))) // { "a" : { "name" : "John" } }
 * println(baseCodec.encode(SubclassB(1, 2))) // { "b" : { "x" : 1, "y" : 2 } }
 * }}}
 */
case object TagToValue extends TagType

/** Specifies that the base codec should wrap the value generated by subclasses'
 * [[com.rojoma.json.codec.JsonCodec]]s in another object containing two fields;
 * one for the type-tag and one for the actual value.
 *
 * @example {{{
 * abstract class Base
 * case class SubclassA(name: String) extends Base
 * case class SubclassB(x: Int, y: Int) extends Base
 *
 * implicit val aCodec = SimpleJsonCodecBuilder[SubclassA].build("name", _.name)
 * implicit val bCodec = SimpleJsonCodecBuilder[SubclassB].build("x", _.x, "y", _.y)
 *
 * val baseCodec = SimpleHierarchyCodecBuilder[Base](TagAndValue("type", "value")).
 *    branch[SubclassA]("a").
 *    branch[SubclassB]("b").
 *    build
 *
 * println(baseCodec.encode(SubclassA("John"))) // { "type" : "a", "value" : { "name" : "John" } }
 * println(baseCodec.encode(SubclassB(1, 2))) // { "type" : "b", "value" : { "x" : 1, "y" : 2 } }
 * }}}
 */
case class TagAndValue(typeField: String, valueField: String) extends TagType {
  if(typeField == valueField) throw new IllegalArgumentException("type field and value field must be different")
}

/** Specifies that the base codec should not affect the subclasses'
 * [[com.rojoma.json.codec.JsonCodec]]s at all and that the decoder should
 * simply try each codec in turn, in the order they were provided to the
 * builder, until it finds one that succeeds.
 *
 * @example {{{
 * abstract class Base
 * case class SubclassA(name: String) extends Base
 * case class SubclassB(x: Int, y: Int) extends Base
 *
 * implicit val aCodec = SimpleJsonCodecBuilder[SubclassA].build("name", _.name)
 * implicit val bCodec = SimpleJsonCodecBuilder[SubclassB].build("x", _.x, "y", _.y)
 *
 * val baseCodec = SimpleHierarchyCodecBuilder[Base](NoTag).
 *    branch[SubclassA].
 *    branch[SubclassB].
 *    build
 *
 * println(baseCodec.encode(SubclassA("John"))) // { "name" : "John" }
 * println(baseCodec.encode(SubclassB(1, 2))) // { "x" : 1, "y" : 2 }
 * }}}
 *
 * @see [[com.rojoma.json.util.TagType]]
 */
sealed abstract class NoTag
case object NoTag extends NoTag

object SimpleHierarchyCodecBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyCodecBuilder[Root](tagType, Map.empty, Map.empty)
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyCodecBuilder[Root](Vector.empty)
}
