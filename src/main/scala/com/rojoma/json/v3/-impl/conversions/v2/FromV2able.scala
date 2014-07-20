package com.rojoma.json.v3
package `-impl`.conversions.v2

import com.rojoma.json.{ast => v2ast}
import com.rojoma.json.v3.{ast => v3ast}
import com.rojoma.json.{io => v2io}
import com.rojoma.json.v3.{io => v3io}

import conversions.v2._

class FromV2Token(val `private once 2.10 is no more`: v2io.JsonToken) extends AnyVal {
  def toV3: v3io.JsonToken = `private once 2.10 is no more` match {
    case t@v2io.TokenOpenBrace() => v3io.TokenOpenBrace()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenCloseBrace() => v3io.TokenCloseBrace()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenOpenBracket() => v3io.TokenOpenBracket()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenCloseBracket() => v3io.TokenCloseBracket()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenColon() => v3io.TokenColon()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenComma() => v3io.TokenComma()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenIdentifier(s) => v3io.TokenIdentifier(s)(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenNumber(n) => v3io.TokenNumber(n.toString)(v3io.Position(t.position.row, t.position.column))
    case t@v2io.TokenString(s) => v3io.TokenString(s)(v3io.Position(t.position.row, t.position.column))
  }
}

class FromV2TokenIterator(val `private once 2.10 is no more`: Iterator[v2io.JsonToken]) extends AnyVal {
  def toV3 = `private once 2.10 is no more`.map(_.toV3)
}

class FromV2Event(val `private once 2.10 is no more`: v2io.JsonEvent) extends AnyVal {
  def toV3: v3io.JsonEvent = `private once 2.10 is no more` match {
    case t@v2io.StartOfObjectEvent() => v3io.StartOfObjectEvent()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.EndOfObjectEvent() => v3io.EndOfObjectEvent()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.StartOfArrayEvent() => v3io.StartOfArrayEvent()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.EndOfArrayEvent() => v3io.EndOfArrayEvent()(v3io.Position(t.position.row, t.position.column))
    case t@v2io.FieldEvent(name) => v3io.FieldEvent(name)(v3io.Position(t.position.row, t.position.column))
    case t@v2io.IdentifierEvent(text) => v3io.IdentifierEvent(text)(v3io.Position(t.position.row, t.position.column))
    case t@v2io.NumberEvent(number) => v3io.NumberEvent(number.toString)(v3io.Position(t.position.row, t.position.column))
    case t@v2io.StringEvent(string) => v3io.StringEvent(string)(v3io.Position(t.position.row, t.position.column))
  }
}

class FromV2EventIterator(val `private once 2.10 is no more`: Iterator[v2io.JsonEvent]) extends AnyVal {
  def toV3 = `private once 2.10 is no more`.map(_.toV3)
}

class FromV2Value(val `private once 2.10 is no more`: v2ast.JValue) extends AnyVal {
  def toV3: v3ast.JValue = `private once 2.10 is no more` match {
    case atom: v2ast.JAtom => new FromV2Atom(atom).toV3
    case compound: v2ast.JCompound => new FromV2Compound(compound).toV3
  }
}

class FromV2Compound(val `private once 2.10 is no more`: v2ast.JCompound) extends AnyVal {
  def toV3: v3ast.JCompound = `private once 2.10 is no more` match {
    case obj: v2ast.JObject => new FromV2Object(obj).toV3
    case arr: v2ast.JArray => new FromV2Array(arr).toV3
  }
}

class FromV2Object(val `private once 2.10 is no more`: v2ast.JObject) extends AnyVal {
  def toV3: v3ast.JObject = v3ast.JObject(`private once 2.10 is no more`.fields.mapValues(new FromV2Value(_).toV3))
}

class FromV2Array(val `private once 2.10 is no more`: v2ast.JArray) extends AnyVal {
  def toV3: v3ast.JArray = v3ast.JArray(`private once 2.10 is no more`.toSeq.view.map(new FromV2Value(_).toV3))
}

class FromV2Atom(val `private once 2.10 is no more`: v2ast.JAtom) extends AnyVal {
  def toV3: v3ast.JAtom = `private once 2.10 is no more` match {
    case s: v2ast.JString => new FromV2String(s).toV3
    case n: v2ast.JNumber => new FromV2Number(n).toV3
    case b: v2ast.JBoolean => new FromV2Boolean(b).toV3
    case n: v2ast.JNull => new FromV2Null(n).toV3
  }
}

class FromV2String(val `private once 2.10 is no more`: v2ast.JString) extends AnyVal {
  def toV3: v3ast.JString = v3ast.JString(`private once 2.10 is no more`.string)
}

class FromV2Number(val `private once 2.10 is no more`: v2ast.JNumber) extends AnyVal {
  def toV3: v3ast.JNumber = v3ast.JNumber(`private once 2.10 is no more`.toBigDecimal)
}

class FromV2Boolean(val `private once 2.10 is no more`: v2ast.JBoolean) extends AnyVal {
  def toV3: v3ast.JBoolean = if(`private once 2.10 is no more`.boolean) v3ast.JBoolean.canonicalTrue else v3ast.JBoolean.canonicalFalse
}

class FromV2Null(val `private once 2.10 is no more`: v2ast.JNull) extends AnyVal {
  def toV3: v3ast.JNull = v3ast.JNull
}
