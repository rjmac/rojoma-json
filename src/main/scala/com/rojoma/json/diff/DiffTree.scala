package com.rojoma.json
package diff

import ast._

sealed abstract class DiffTree
case class ObjectDiff(fieldDiffs: Map[String, DiffTree]) extends DiffTree
case class ArrayDiff(elementDiffs: Map[Int, DiffTree]) extends DiffTree
case class Addition(newValue: JValue) extends DiffTree
case class Replacement(oldValue: JValue, newValue: JValue) extends DiffTree
case class Removal(oldValue: JValue) extends DiffTree
