package com.rojoma.json
package codec

import ast._

import scala.{collection => sc}
import sc.JavaConversions._
import sc.{mutable => scm}
import java.{util => ju}

private[codec] object CBHolder { // want people to do "import JsonCodecs._" without polluting their namespace with non-codecs
  type CB[A, B] = sc.generic.CanBuild[A, B]
}

/** Generally-useful json implicits. */
object JsonCodecs {
  import CBHolder._

  // I would like all of these codecs' encode methods to use view, but unfortunately in scala 2.8.1,
  // empty views and non-views are not symmetrically comparable (Nil.view == Nil, but Nil != Nil.view).  This
  // is fixed in 2.9.  Once we've migrated there, I think these should become views again.
  //
  // I've marked all the lines which "should" be views with "VIEWIFY"

  implicit def seqCodec[T, S[X] <: sc.Seq[X]](implicit tCodec: JsonCodec[T], buildFactory: CB[T, S[T]]) = new JsonCodec[S[T]] {
    def encode(x: S[T]): JValue = {
      JArray(x.map(implicitly[JsonCodec[T]].encode)) // VIEWIFY
    }

    def decode(xs: JValue): Option[S[T]] = xs match {
      case JArray(jElems) =>
        val builder = buildFactory()
        val subCodec = implicitly[JsonCodec[T]]
        for (jElem <- jElems) {
          subCodec.decode(jElem) match {
            case Some(elem) =>
              builder += elem
            case None =>
              return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def arrayCodec[T: JsonCodec: ClassManifest] = new JsonCodec[Array[T]] {
    def encode(x: Array[T]): JValue =
      JArray(x.map(implicitly[JsonCodec[T]].encode)) // VIEWIFY

    def decode(xs: JValue): Option[Array[T]] = xs match {
      case JArray(jElems) =>
        val builder = scm.ArrayBuilder.make[T]()
        val subCodec = implicitly[JsonCodec[T]]
        for(jElem <- jElems) {
          subCodec.decode(jElem) match {
            case Some(elem) =>
              builder += elem
            case None =>
              return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def juListCodec[T: JsonCodec] = new JsonCodec[ju.List[T]] {
    def encode(x: ju.List[T]): JValue = {
      JArray(x.map(implicitly[JsonCodec[T]].encode)) // VIEWIFY
    }

    def decode(xs: JValue): Option[ju.List[T]] = xs match {
      case JArray(jElems) =>
        val result = new ju.ArrayList[T]
        val subCodec = implicitly[JsonCodec[T]]
        for(jElem <- jElems) {
          subCodec.decode(jElem) match {
            case Some(elem) =>
              result.add(elem)
            case None =>
              return None
          }
        }
        Some(result)
      case _ =>
        None
    }
  }

  implicit object stringCodec extends JsonCodec[String] {
    def encode(x: String) = JString(x)
    def decode(x: JValue) = x match {
      case JString(s) => Some(s)
      case _ => None
    }
  }

  implicit object boolCodec extends JsonCodec[Boolean] {
    def encode(x: Boolean) = JBoolean(x)
    def decode(x: JValue) = x match {
      case JBoolean(b) => Some(b)
      case _ => None
    }
  }

  implicit object jvalueCodec extends JsonCodec[JValue] {
    def encode(x: JValue) = x
    def decode(x: JValue) = Some(x)
  }

  implicit def mapCodec[T, M[U, V] <: sc.Map[U, V]](implicit tCodec: JsonCodec[T], buildFactory: CB[(String, T), M[String, T]]) = new JsonCodec[M[String, T]] {
    def encode(x: M[String, T]) =
      JObject(x.mapValues(implicitly[JsonCodec[T]].encode))

    def decode(x: JValue): Option[M[String, T]] = x match {
      case JObject(fields) =>
        val builder = buildFactory()
        val subCodec = implicitly[JsonCodec[T]]
        for((k, jv) <- fields) {
          subCodec.decode(jv) match {
            case Some(v) => builder += (k -> v)
            case None => return None
          }
        }
        Some(builder.result())
      case _ =>
        None
    }
  }

  implicit def juMapCodec[T: JsonCodec] = new JsonCodec[ju.Map[String, T]] {
    def encode(x: ju.Map[String, T]) =
      JObject(x.mapValues(implicitly[JsonCodec[T]].encode))


    def decode(x: JValue): Option[ju.Map[String, T]] = x match {
      case JObject(fields) =>
        val result = new ju.LinkedHashMap[String, T]
        val subCodec = implicitly[JsonCodec[T]]
        for((k, jv) <- fields) {
          subCodec.decode(jv) match {
            case Some(v) => result.put(k, v)
            case None => return None
          }
        }
        Some(result)
      case _ =>
        None
    }
  }

  // either is right-biased; if decoding as Right fails it tries Left;
  // if Left fails the whole thing fails.
  implicit def eitherCodec[L: JsonCodec, R: JsonCodec] = new JsonCodec[Either[L, R]] {
    def encode(x: Either[L,R]) = x match {
      case Left(left) => implicitly[JsonCodec[L]].encode(left)
      case Right(right) => implicitly[JsonCodec[R]].encode(right)
    }

    def decode(x: JValue) = implicitly[JsonCodec[R]].decode(x) orElse implicitly[JsonCodec[L]].decode(x)
  }
}
