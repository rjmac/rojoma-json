package com.rojoma.json.v3
package `-impl`.conversions.v2

import com.rojoma.json.{ast => v2ast}
import com.rojoma.json.v3.{ast => v3ast}
import com.rojoma.json.{io => v2io}
import com.rojoma.json.v3.{io => v3io}

import conversions.v2._

class ToV2Token(val `private once 2.10 is no more`: v3io.JsonToken) extends AnyVal {
  def toV2: v2io.JsonToken = `private once 2.10 is no more` match {
    case t@v3io.TokenOpenBrace() => p(v2io.TokenOpenBrace(), t.position)
    case t@v3io.TokenCloseBrace() => p(v2io.TokenCloseBrace(), t.position)
    case t@v3io.TokenOpenBracket() => p(v2io.TokenOpenBracket(), t.position)
    case t@v3io.TokenCloseBracket() => p(v2io.TokenCloseBracket(), t.position)
    case t@v3io.TokenColon() => p(v2io.TokenColon(), t.position)
    case t@v3io.TokenComma() => p(v2io.TokenComma(), t.position)
    case t@v3io.TokenIdentifier(s) => p(v2io.TokenIdentifier(s), t.position)
    case t@v3io.TokenNumber(n) => p(v2io.TokenNumber(BigDecimal(n, java.math.MathContext.UNLIMITED)), t.position)
    case t@v3io.TokenString(s) => p(v2io.TokenString(s), t.position)
  }

  private def p(t: v2io.JsonToken, pos: v3io.Position): t.type = { t.position = v2io.Position(pos.row, pos.column); t }
}

class ToV2TokenIterator(val `private once 2.10 is no more`: Iterator[v3io.JsonToken]) extends AnyVal {
  def toV2 = `private once 2.10 is no more`.map(_.toV2)
}

class ToV2Event(val `private once 2.10 is no more`: v3io.JsonEvent) extends AnyVal {
  def toV2: v2io.JsonEvent = `private once 2.10 is no more` match {
    case t@v3io.StartOfObjectEvent() => p(v2io.StartOfObjectEvent(), t.position)
    case t@v3io.EndOfObjectEvent() => p(v2io.EndOfObjectEvent(), t.position)
    case t@v3io.StartOfArrayEvent() => p(v2io.StartOfArrayEvent(), t.position)
    case t@v3io.EndOfArrayEvent() => p(v2io.EndOfArrayEvent(), t.position)
    case t@v3io.FieldEvent(name) => p(v2io.FieldEvent(name), t.position)
    case t@v3io.IdentifierEvent(text) => p(v2io.IdentifierEvent(text), t.position)
    case t@v3io.NumberEvent(number) => p(v2io.NumberEvent(BigDecimal(number, java.math.MathContext.UNLIMITED)), t.position)
    case t@v3io.StringEvent(string) => p(v2io.StringEvent(string), t.position)
  }

  private def p(t: v2io.JsonEvent, pos: v3io.Position): t.type = { t.position = v2io.Position(pos.row, pos.column); t }
}

class ToV2EventIterator(val `private once 2.10 is no more`: Iterator[v3io.JsonEvent]) extends AnyVal {
  def toV2 = `private once 2.10 is no more`.map(_.toV2)
}

class ToV2Value(val `private once 2.10 is no more`: v3ast.JValue) extends AnyVal {
  def toV2: v2ast.JValue = `private once 2.10 is no more` match {
    case atom: v3ast.JAtom => new ToV2Atom(atom).toV2
    case compound: v3ast.JCompound => new ToV2Compound(compound).toV2
  }
}

class ToV2Compound(val `private once 2.10 is no more`: v3ast.JCompound) extends AnyVal {
  def toV2: v2ast.JCompound = `private once 2.10 is no more` match {
    case obj: v3ast.JObject => new ToV2Object(obj).toV2
    case arr: v3ast.JArray => new ToV2Array(arr).toV2
  }
}

class ToV2Object(val `private once 2.10 is no more`: v3ast.JObject) extends AnyVal {
  def toV2 = v2ast.JObject(`private once 2.10 is no more`.toMap.mapValues(new ToV2Value(_).toV2))
}

class ToV2Array(val `private once 2.10 is no more`: v3ast.JArray) extends AnyVal {
  def toV2 = v2ast.JArray(`private once 2.10 is no more`.toSeq.view.map(new ToV2Value(_).toV2))
}

class ToV2Atom(val `private once 2.10 is no more`: v3ast.JAtom) extends AnyVal {
  def toV2: v2ast.JAtom = `private once 2.10 is no more` match {
    case s: v3ast.JString => new ToV2String(s).toV2
    case n: v3ast.JNumber => new ToV2Number(n).toV2
    case b: v3ast.JBoolean => new ToV2Boolean(b).toV2
    case n: v3ast.JNull => new ToV2Null(n).toV2
  }
}

class ToV2String(val `private once 2.10 is no more`: v3ast.JString) extends AnyVal {
  def toV2: v2ast.JString = v2ast.JString(`private once 2.10 is no more`.string)
}

class ToV2Number(val `private once 2.10 is no more`: v3ast.JNumber) extends AnyVal {
  def toV2: v2ast.JNumber = v2ast.JNumber(`private once 2.10 is no more`.toBigDecimal)
}

class ToV2Boolean(val `private once 2.10 is no more`: v3ast.JBoolean) extends AnyVal {
  def toV2: v2ast.JBoolean = if(`private once 2.10 is no more`.boolean) v2ast.JBoolean.canonicalTrue else v2ast.JBoolean.canonicalFalse
}

class ToV2Null(val `private once 2.10 is no more`: v3ast.JNull) extends AnyVal {
  def toV2: v2ast.JNull = v2ast.JNull
}
