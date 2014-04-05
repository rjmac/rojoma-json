package com.rojoma
package json.v2
package codec

import ast._

sealed abstract class JsonValidation[+T] {
  def map[U](f: T => U): JsonValidation[U]
  def flatMap[U](f: T => JsonValidation[U]): JsonValidation[U]
  def foreach[U](f: T => U): Unit

  def at(pathComponent: PathComponent): JsonValidation[T]
}

abstract class Failure extends JsonValidation[Nothing] {
  val path: Seq[PathComponent]

  def map[U](f: Nothing => U) = this
  def flatMap[U](f: Nothing => JsonValidation[U]) = this
  def foreach[U](f: Nothing => U) = {}
}

sealed abstract class PathComponent
case class ArrayPathComponent(index: Int)
case class ObjectPathComponent(key: String)

case class UnexpectedType(expected: Set[JValueType], got: JValueType, path: Seq[PathComponent] = Vector.empty) extends Failure {
  def at(pathComponent: PathComponent) = copy(path = pathComponent +: path)
}

case class MissingField(fieldName: String, path: Seq[PathComponent] = Vector.empty) extends Failure {
  def at(pathComponent: PathComponent) = copy(path = pathComponent +: path)
}

case class Success[+T](result: T) extends JsonValidation[T] {
  def map[U](f: T => U) = Success(f(result))
  def flatMap[U](f: T => JsonValidation[U]) = f(result)
  def foreach[U](f: T => U): Unit = f(result)

  def at(pathComponent: PathComponent): this.type = this
}
