package com.rojoma.json.v3
package `-impl`.conversions.v2

import com.rojoma.json.{ast => v2}
import com.rojoma.json.v3.{ast => v3}

class FromV2Value(val `private once 2.10 is no more`: v2.JValue) extends AnyVal {
  def toV3: v3.JValue = `private once 2.10 is no more` match {
    case atom: v2.JAtom => new FromV2Atom(atom).toV3
    case compound: v2.JCompound => new FromV2Compound(compound).toV3
  }
}

class FromV2Compound(val `private once 2.10 is no more`: v2.JCompound) extends AnyVal {
  def toV3: v3.JCompound = `private once 2.10 is no more` match {
    case obj: v2.JObject => new FromV2Object(obj).toV3
    case arr: v2.JArray => new FromV2Array(arr).toV3
  }
}

class FromV2Object(val `private once 2.10 is no more`: v2.JObject) extends AnyVal {
  def toV3: v3.JObject = v3.JObject(`private once 2.10 is no more`.fields.mapValues(new FromV2Value(_).toV3))
}

class FromV2Array(val `private once 2.10 is no more`: v2.JArray) extends AnyVal {
  def toV3: v3.JArray = v3.JArray(`private once 2.10 is no more`.toSeq.view.map(new FromV2Value(_).toV3))
}

class FromV2Atom(val `private once 2.10 is no more`: v2.JAtom) extends AnyVal {
  def toV3: v3.JAtom = `private once 2.10 is no more` match {
    case s: v2.JString => new FromV2String(s).toV3
    case n: v2.JNumber => new FromV2Number(n).toV3
    case b: v2.JBoolean => new FromV2Boolean(b).toV3
    case n: v2.JNull => new FromV2Null(n).toV3
  }
}

class FromV2String(val `private once 2.10 is no more`: v2.JString) extends AnyVal {
  def toV3: v3.JString = v3.JString(`private once 2.10 is no more`.string)
}

class FromV2Number(val `private once 2.10 is no more`: v2.JNumber) extends AnyVal {
  def toV3: v3.JNumber = v3.JNumber(`private once 2.10 is no more`.toBigDecimal)
}

class FromV2Boolean(val `private once 2.10 is no more`: v2.JBoolean) extends AnyVal {
  def toV3: v3.JBoolean = if(`private once 2.10 is no more`.boolean) v3.JBoolean.canonicalTrue else v3.JBoolean.canonicalFalse
}

class FromV2Null(val `private once 2.10 is no more`: v2.JNull) extends AnyVal {
  def toV3: v3.JNull = v3.JNull
}
