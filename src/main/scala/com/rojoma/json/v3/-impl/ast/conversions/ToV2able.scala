package com.rojoma.json.v3
package `-impl`.ast.conversions

import com.rojoma.json.{ast => v2}
import com.rojoma.json.v3.{ast => v3}

class ToV2Value(val `private once 2.10 is no more`: v3.JValue) extends AnyVal {
  def toV2: v2.JValue = `private once 2.10 is no more` match {
    case atom: v3.JAtom => new ToV2Atom(atom).toV2
    case compound: v3.JCompound => new ToV2Compound(compound).toV2
  }
}

class ToV2Compound(val `private once 2.10 is no more`: v3.JCompound) extends AnyVal {
  def toV2: v2.JCompound = `private once 2.10 is no more` match {
    case obj: v3.JObject => new ToV2Object(obj).toV2
    case arr: v3.JArray => new ToV2Array(arr).toV2
  }
}

class ToV2Object(val `private once 2.10 is no more`: v3.JObject) extends AnyVal {
  def toV2 = v2.JObject(`private once 2.10 is no more`.toMap.mapValues(new ToV2Value(_).toV2))
}

class ToV2Array(val `private once 2.10 is no more`: v3.JArray) extends AnyVal {
  def toV2 = v2.JArray(`private once 2.10 is no more`.toSeq.view.map(new ToV2Value(_).toV2))
}

class ToV2Atom(val `private once 2.10 is no more`: v3.JAtom) extends AnyVal {
  def toV2: v2.JAtom = `private once 2.10 is no more` match {
    case s: v3.JString => new ToV2String(s).toV2
    case n: v3.JNumber => new ToV2Number(n).toV2
    case b: v3.JBoolean => new ToV2Boolean(b).toV2
    case n: v3.JNull => new ToV2Null(n).toV2
  }
}

class ToV2String(val `private once 2.10 is no more`: v3.JString) extends AnyVal {
  def toV2: v2.JString = v2.JString(`private once 2.10 is no more`.string)
}

class ToV2Number(val `private once 2.10 is no more`: v3.JNumber) extends AnyVal {
  def toV2: v2.JNumber = v2.JNumber(`private once 2.10 is no more`.toBigDecimal)
}

class ToV2Boolean(val `private once 2.10 is no more`: v3.JBoolean) extends AnyVal {
  def toV2: v2.JBoolean = if(`private once 2.10 is no more`.boolean) v2.JBoolean.canonicalTrue else v2.JBoolean.canonicalFalse
}

class ToV2Null(val `private once 2.10 is no more`: v3.JNull) extends AnyVal {
  def toV2: v2.JNull = v2.JNull
}
