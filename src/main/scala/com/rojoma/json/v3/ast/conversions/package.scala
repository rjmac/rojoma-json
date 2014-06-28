package com.rojoma.json.v3
package ast

import scala.language.implicitConversions

import com.rojoma.json.{ast => v2}

import `-impl`.ast.conversions._

package object conversions extends LowPriorityConversions {
  implicit def toNumberConversion(x: JNumber) = new ToV2Number(x)
  implicit def toStringConversion(x: JString) = new ToV2String(x)
  implicit def toBooleanConversion(x: JBoolean) = new ToV2Boolean(x)
  implicit def toNullConversion(x: JNull) = new ToV2Null(x)
  implicit def toObjectConversion(x: JObject) = new ToV2Object(x)
  implicit def toArrayConversion(x: JArray) = new ToV2Array(x)

  implicit def fromNumberConversion(x: v2.JNumber) = new FromV2Number(x)
  implicit def fromStringConversion(x: v2.JString) = new FromV2String(x)
  implicit def fromBooleanConversion(x: v2.JBoolean) = new FromV2Boolean(x)
  implicit def fromNullConversion(x: v2.JNull) = new FromV2Null(x)
  implicit def fromObjectConversion(x: v2.JObject) = new FromV2Object(x)
  implicit def fromArrayConversion(x: v2.JArray) = new FromV2Array(x)
}
