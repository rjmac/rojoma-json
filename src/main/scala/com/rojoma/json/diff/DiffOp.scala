package com.rojoma.json
package diff

import ast.JValue

sealed abstract class DiffOp
case class Addition(newValue: JValue) extends DiffOp
case class Replacement(oldValue: JValue, newValue: JValue) extends DiffOp
case class Removal(oldValue: JValue) extends DiffOp
