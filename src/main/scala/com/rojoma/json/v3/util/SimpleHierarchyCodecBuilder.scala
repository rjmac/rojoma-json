package com.rojoma.json.v3
package util

import scala.language.existentials
import scala.reflect.ClassTag

import ast._
import codec._

import com.rojoma.json.v3.`-impl`.util.ClassAwareMap

class SimpleHierarchyCodecBuilder[Root <: AnyRef] private[util] (enc: SimpleHierarchyEncodeBuilder[Root], dec: SimpleHierarchyDecodeBuilder[Root]) {
  def branch[T <: Root : JsonEncode : JsonDecode : ClassTag](name: String) = {
    new SimpleHierarchyCodecBuilder[Root](enc.branch[T](name), dec.branch[T](name))
  }

  def build: JsonEncode[Root] with JsonDecode[Root] = {
    new JsonEncode[Root] with JsonDecode[Root] {
      val e = enc.build
      val d = dec.build
      def encode(x: Root) = e.encode(x)
      def decode(x: JValue) = d.decode(x)
    }
  }
}

class NoTagSimpleHierarchyCodecBuilder[Root <: AnyRef] private[util] (enc: NoTagSimpleHierarchyEncodeBuilder[Root], dec: NoTagSimpleHierarchyDecodeBuilder[Root]) {
  def branch[T <: Root : JsonEncode : JsonDecode : ClassTag] = {
    new NoTagSimpleHierarchyCodecBuilder[Root](enc.branch[T], dec.branch[T])
  }

  def build: JsonEncode[Root] with JsonDecode[Root] = {
    new JsonEncode[Root] with JsonDecode[Root] {
      val e = enc.build
      val d = dec.build
      def encode(x: Root) = e.encode(x)
      def decode(x: JValue) = d.decode(x)
    }
  }
}

object SimpleHierarchyCodecBuilder {
  def apply[Root <: AnyRef](tagType: TagType) = new SimpleHierarchyCodecBuilder[Root](SimpleHierarchyEncodeBuilder[Root](tagType), SimpleHierarchyDecodeBuilder[Root](tagType))
  def apply[Root <: AnyRef](tagType: NoTag) = new NoTagSimpleHierarchyCodecBuilder[Root](SimpleHierarchyEncodeBuilder[Root](tagType), SimpleHierarchyDecodeBuilder[Root](tagType))
}
