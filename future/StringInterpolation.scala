package com.rojoma.json
package future

import io._
import ast._
import codec._

object implicits {
  implicit def sc2jsc(sc: StringContext) = new JsonStringContext(sc)
}

case class Jsonized(value: JValue)

object Jsonized {
  implicit def any2jsonized[T: JsonCodec](x: T): Jsonized = Jsonized(implicitly[JsonCodec[T]].encode(x))
}

class JsonStringContext(xs: StringContext) {
  // Even better: with some work this can be made a macro, which means
  // compile-time checking of the validity of the format.  I think it
  // could also get rid of Jsonized in favor of inserting JsonCodec.toJValue
  // calls itself, so values would become Any* (and using a not-String in
  // field-name position would get a better error message too, not to mention
  // being compile-time typechecked).
  def json(values: Jsonized*): JValue = {
    assert(values.length == xs.parts.length - 1)
    val toInsert = values.iterator.map { jsonized =>
      toTokenStream(jsonized.value)
    }
    val literalTokens = xs.parts.iterator.map { part => new TokenIterator(new java.io.StringReader(part)).map(_.token) }
    new JsonReader(intercalate(literalTokens, toInsert).flatten.map(new PositionedJsonToken(_, -1, -1))).read()
  }

  def toTokenStream(value: JValue): Iterator[JsonToken] = value match {
    case JString(text) => Iterator.single(TokenString(text))
    case JNumber(num) => Iterator.single(TokenNumber(num))
    case JBoolean(b) => Iterator.single(TokenIdentifier(b.toString))
    case JNull => Iterator.single(TokenIdentifier("null"))
    case JObject(o) =>
      val fields = o.iterator.map {
        case (k, v) => Seq(TokenString(k), TokenColon).iterator ++ toTokenStream(v)
      }
      Iterator.single(TokenOpenBrace) ++ intersperse(fields, TokenComma) ++ Iterator.single(TokenCloseBrace)
    case JArray(a) =>
      val elems = a.iterator.map(toTokenStream)
      Iterator.single(TokenOpenBracket) ++ intersperse(elems, TokenComma) ++ Iterator.single(TokenCloseBracket)
  }

  def intersperse[T](items: Iterator[Iterator[T]], inBetween: T): Iterator[T] = new Iterator[T] {
    var current: Iterator[T] = 
      if(items.hasNext) items.next()
      else Iterator.empty

    def hasNext = items.hasNext || current.hasNext

    def next() = {
      if(current.hasNext) {
        current.next()
      } else {
        current = items.next()
        inBetween
      }
    }
  }

  def intercalate[T](items: Iterator[T], inBetween: Iterator[T]): Iterator[T] = new Iterator[T] {
    // preconditoin: inBetween.length < items.length
    var nextIsInBetween = false
    def hasNext = items.hasNext
    def next() = {
      nextIsInBetween = !nextIsInBetween
      if(nextIsInBetween) items.next()
      else inBetween.next()
    }
  }
}
