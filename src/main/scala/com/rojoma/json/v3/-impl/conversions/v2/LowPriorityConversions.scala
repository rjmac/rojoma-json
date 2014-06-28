package com.rojoma.json.v3
package `-impl`.conversions.v2

import scala.language.implicitConversions

import com.rojoma.json.{ast => v2}
import com.rojoma.json.v3.{ast => v3}

class LowerPriorityConversions {
  implicit def toValueConversion(x: v3.JValue) = new ToV2Value(x)

  implicit def fromValueConversion(x: v2.JValue) = new FromV2Value(x)
}

class LowPriorityConversions extends LowerPriorityConversions {
  implicit def toAtomConversion(x: v3.JAtom) = new ToV2Atom(x)
  implicit def toCompoundConversion(x: v3.JCompound) = new ToV2Compound(x)

  implicit def fromAtomConversion(x: v2.JAtom) = new FromV2Atom(x)
  implicit def fromCompoundConversion(x: v2.JCompound) = new FromV2Compound(x)
}
