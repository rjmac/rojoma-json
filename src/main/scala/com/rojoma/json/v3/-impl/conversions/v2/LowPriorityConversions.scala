package com.rojoma.json.v3
package `-impl`.conversions.v2

import scala.language.implicitConversions

import com.rojoma.json.{ast => v2ast}
import com.rojoma.json.v3.{ast => v3ast}
import com.rojoma.json.{io => v2io}
import com.rojoma.json.v3.{io => v3io}

class LowerPriorityConversions {
  implicit def toValueConversion(x: v3ast.JValue) = new ToV2Value(x)

  implicit def fromValueConversion(x: v2ast.JValue) = new FromV2Value(x)
}

class LowPriorityConversions extends LowerPriorityConversions {
  implicit def toAtomConversion(x: v3ast.JAtom) = new ToV2Atom(x)
  implicit def toCompoundConversion(x: v3ast.JCompound) = new ToV2Compound(x)

  implicit def fromAtomConversion(x: v2ast.JAtom) = new FromV2Atom(x)
  implicit def fromCompoundConversion(x: v2ast.JCompound) = new FromV2Compound(x)

  implicit def toTokenConversion(x: v3io.JsonToken) = new ToV2Token(x)
  implicit def toTokenIteratorConversion(x: Iterator[v3io.JsonToken]) = new ToV2TokenIterator(x)
  implicit def toEventConversion(x: v3io.JsonEvent) = new ToV2Event(x)
  implicit def toEventIteratorConversion(x: Iterator[v3io.JsonEvent]) = new ToV2EventIterator(x)

  implicit def fromTokenConversion(x: v2io.JsonToken) = new FromV2Token(x)
  implicit def fromTokenIteratorConversion(x: Iterator[v2io.JsonToken]) = new FromV2TokenIterator(x)
  implicit def fromEventConversion(x: v2io.JsonEvent) = new FromV2Event(x)
  implicit def fromEventIteratorConversion(x: Iterator[v2io.JsonEvent]) = new FromV2EventIterator(x)
}
