package com.rojoma.`json-impl`.ast

sealed abstract class DynamicDisambiguate
case object NotProvided extends DynamicDisambiguate
case class Index(idx: Int) extends DynamicDisambiguate
case class Field(field: String) extends DynamicDisambiguate

object DynamicDisambiguate {
  implicit def idx2dd(idx: Int): DynamicDisambiguate = Index(idx)
  implicit def field2dd(field: String): DynamicDisambiguate = Field(field)
}
