package com.rojoma.json.v3
package `-impl`.interpolation

import scala.quoted._
import scala.collection.immutable.{VectorBuilder, VectorMap, ArraySeq}
import scala.collection.mutable
import java.io.StringReader

import ast._
import io._

object JsonInterpolatorImpl {
  def j(strCtxExpr: Expr[StringContext], piecesExpr: Expr[Seq[Any]])(using Quotes): Expr[JValue] = {
    import quotes.reflect._

    case class Bail(msg: String, pos: Position) extends Exception(msg)
    def bail(msg: String, pos: Position) = throw Bail(msg, pos)

    def noCodecFound[T](term: Term, replace: => Expr[T]): Expr[T] = {
      report.error("No JsonEncode instance found for this term", term.pos)
      replace
    }
    def typeMismatch[T](term: Term, expected: String, replace: => Expr[T]): Expr[T] = {
      report.throwError(s"Type mismatch: expected $expected", term.pos)
      replace
    }

    def encodeExpr(expr: Expr[Any]): Expr[JValue] = {
      val term = expr.asTerm
      if(term.tpe <:< TypeRepr.of[JValue]) {
        term.asExprOf[JValue]
      } else {
        term.tpe.widen.asType match {
          case '[a] =>
            Expr.summon[codec.JsonEncode[a]] match {
              case Some(encoder) =>
                val typedExpr = term.asExprOf[a]
                '{ $encoder.encode($typedExpr) }
              case None =>
                noCodecFound(term, '{ JNull })
            }
          }
      }
    }

    def encodeField(expr: Expr[Any]): Expr[String] = {
      val term = expr.asTerm
      term.tpe.widen.asType match {
        case '[a] =>
          Expr.summon[codec.FieldEncode[a]] match {
            case Some(encoder) =>
              val typedExpr = term.asExprOf[a]
              '{ $encoder.encode($typedExpr) }
            case None =>
              noCodecFound(term, '{ "" })
          }
      }
    }

    def encodeOptional(expr: Expr[Any]): Expr[Option[JValue]] = {
      val term = expr.asTerm
      term.tpe.widen.asType match {
        case '[Option[a]] =>
          Expr.summon[codec.JsonEncode[a]] match {
            case Some(encoder) =>
              val typedExpr = term.asExprOf[Option[a]]
              '{ Convert.option($typedExpr)(using $encoder) }
            case None =>
              noCodecFound(term, '{ Option.empty[JValue] })
          }
        case _ =>
          typeMismatch(term, "Option", '{ Option.empty[JValue] })
      }
    }

    def encodeArray(expr: Expr[Any]): Expr[Iterator[JValue]] = {
      val term = expr.asTerm
      term.tpe.widen.asType match {
        case '[Iterable[a]] =>
          Expr.summon[codec.JsonEncode[a]] match {
            case Some(encoder) =>
              val typedExpr = term.asExprOf[Iterable[a]]
              '{ Convert.array($typedExpr)(using $encoder) }
            case None =>
              noCodecFound(term, '{ Iterator.empty[JValue] })
          }
        case _ =>
          typeMismatch(term, "Iterable", '{ Iterator.empty[JValue] })
      }
    }

    def encodeMap(expr: Expr[Any]): Expr[Iterator[(String, JValue)]] = {
      val term = expr.asTerm
      term.tpe.widen.asType match {
        case '[Iterable[(a, b)]] =>
          (Expr.summon[codec.FieldEncode[a]], Expr.summon[codec.JsonEncode[b]]) match {
            case (Some(aEncoder), Some(bEncoder)) =>
              val typedExpr = term.asExprOf[Iterable[(a, b)]]
              '{ Convert.map($typedExpr)(using $aEncoder, $bEncoder) }
            case _ =>
              noCodecFound(term, '{ Iterator.empty[(String, JValue)] })
          }
        case _ =>
          typeMismatch(term, "Iterable of pairs", '{ Iterator.empty[(String, JValue)] })
      }
    }

    sealed abstract class Tokenized {
      def position: Position
    }
    case class Token(token: JsonToken)(pos: => Position) extends Tokenized {
      lazy val position = pos
    }
    case class Unquote(expr: Expr[Any]) extends Tokenized {
      def position = expr.asTerm.pos
    }
    object Unquote extends (Expr[Any] => Tokenized)
    case class UnquoteSplice(expr: Expr[Any]) extends Tokenized {
      def position = expr.asTerm.pos
    }
    object UnquoteSplice extends (Expr[Any] => Tokenized)
    case class UnquoteOptional(expr: Expr[Any]) extends Tokenized {
      def position = expr.asTerm.pos
    }
    object UnquoteOptional extends (Expr[Any] => Tokenized)
    case class DeferredError(position: Position, message: String) extends Tokenized
    case class End(position: Position) extends Tokenized

    sealed trait State {
      def step(thing: Tokenized): Either[State, Expr[JValue]]
    }

    def unexpectedTokenized(thing: Tokenized, expecting: String): Nothing =
      thing match {
        case Token(t) =>
          bail(stripPos(new JsonUnexpectedToken(t, expecting).message), thing.position)
        case DeferredError(pos, what) =>
          bail(what, pos)
        case End(pos) =>
          bail(new JsonParserEOF(io.Position.Invalid).message, pos)
        case UnquoteSplice(_) =>
          bail(s"Expected $expecting; got splicing unquote", thing.position)
        case UnquoteOptional(_) =>
          bail(s"Expected $expecting; got optional unquote", thing.position)
        case Unquote(t) =>
          bail(s"Expected $expecting; got unquote", thing.position)
      }

    def withThing(thing: Tokenized, expecting: String)(f: PartialFunction[Tokenized, Either[State, Expr[JValue]]]): Either[State, Expr[JValue]] =
      f.applyOrElse(thing, unexpectedTokenized(_ : Tokenized, expecting))

    sealed abstract class ArrayCtor {
      def populator: Expr[VectorBuilder[JValue]] => Expr[Any]
    }
    case class UnknownCountArrayCtor(populator: Expr[VectorBuilder[JValue]] => Expr[Any]) extends ArrayCtor
    case class OneItemCtor(value: Expr[JValue]) extends ArrayCtor {
      def populator = (e: Expr[VectorBuilder[JValue]]) => '{ $e += $value }
    }

    type VectorMapBuilder[K, V] = mutable.Builder[(K, V), VectorMap[K, V]]
    type ObjectCtor = Expr[VectorMapBuilder[String, JValue]] => Expr[Any]

    def finishArray(ctors: List[ArrayCtor]): Expr[JValue] = {
      if(ctors.isEmpty) '{ JArray.empty }
      else if(ctors.forall(_.isInstanceOf[OneItemCtor])) {
        val singletons = ctors.map(_.asInstanceOf[OneItemCtor])
        '{
           val temp = new Array[JValue](${Expr(ctors.length)})
           ${
             Block(
               singletons.reverse.zipWithIndex.map { case (ctor, i) =>
                 '{ temp(${Expr(i)}) = ${ctor.value} }.asTerm },
               '{ JArray(ArraySeq.unsafeWrapArray(temp)) }.asTerm
             ).asExprOf[JArray]
           }
        }
      } else {
        '{
           val temp = new VectorBuilder[JValue]
           ${Block(ctors.reverse.map { ctor => ctor.populator('{temp}).asTerm }, '{ JArray(temp.result()) }.asTerm).asExprOf[JArray]}
         }
      }
    }

    def finishObject(ctors: List[ObjectCtor]): Expr[JValue] = {
      if(ctors.isEmpty) '{ JObject.empty }
      else {
        '{
           val temp = VectorMap.newBuilder[String, JValue]
           ${Block(ctors.reverse.map { ctor => ctor('{temp}).asTerm }, '{ JObject(temp.result()) }.asTerm).asExprOf[JObject]}
        }
      }
    }

    def arrayItem(v: Expr[Any]): ArrayCtor =
      OneItemCtor(encodeExpr(v))

    def optionalArrayItem(v: Expr[Any]): ArrayCtor =
      UnknownCountArrayCtor(builder => '{ $builder ++= ${encodeOptional(v)} })

    def arrayItems(v: Expr[Any]): ArrayCtor =
      UnknownCountArrayCtor(builder => '{ $builder ++= ${encodeArray(v)} })

    def objectItem(k: Expr[String], v: Expr[Any]): ObjectCtor = { builder =>
      '{ $builder += (($k, ${encodeExpr(v)})) }
    }

    def optionalObjectItem(k: Expr[String], v: Expr[Any]): ObjectCtor = { builder =>
      '{
         val kTemp = $k
         ${encodeOptional(v)} match {
           case Some(vTemp) => $builder += ((kTemp, vTemp))
           case None => {}
         }
      }
    }

    def objectItems(v: Expr[Any]): ObjectCtor = { builder =>
      '{ $builder ++= ${encodeMap(v)} }
    }

    class TopLevelDatum extends State {
      def step(thing: Tokenized) =
        new ExpectingDatum(new ExpectingEnd(_)).step(thing)
    }

    class ExpectingEnd(result: Expr[JValue]) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "end of input") {
          case End(_) => Right(result)
        }
    }

    class ExpectingDatum(returnState: Expr[JValue] => State, expecting: String = "datum") extends State {
      def step(thing: Tokenized) =
        withThing(thing, expecting) {
          case Unquote(item) =>
            Left(returnState(encodeExpr(item)))
          case Token(TokenOpenBracket()) =>
            Left(new ExpectingDatumOrEndOfArray(Nil, returnState))
          case Token(TokenOpenBrace()) =>
            Left(new ExpectingFieldOrEndOfObject(Nil, returnState))
          case Token(TokenString(s)) =>
            Left(returnState('{ JString(${Expr(s)}) }))
          case Token(TokenIdentifier("true")) =>
            Left(returnState('{ JBoolean.canonicalTrue }))
          case Token(TokenIdentifier("false")) =>
            Left(returnState('{ JBoolean.canonicalFalse }))
          case Token(TokenIdentifier("null")) =>
            Left(returnState('{ JNull }))
          case Token(TokenNumber(n)) =>
            // Hmm.   We can inspect "n" and generate a more specific JNumber type.
            // But it's most likely we'll just be serializing the result, so there's
            // not much point.
            Left(returnState('{ JNumber.unsafeFromString(${Expr(n)}) }))
        }
    }

    class ExpectingDatumOrEndOfArray(ctors: List[ArrayCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case Token(TokenCloseBracket()) =>
            Left(returnState(finishArray(ctors)))
          case other =>
            new ExpectingArrayDatum(ctors, returnState, "datum or end of array").step(other)
        }
    }

    class ExpectingArrayDatum(ctors: List[ArrayCtor], returnState: Expr[JValue] => State, expecting: String) extends State {
      def step(thing: Tokenized) =
        thing match {
          case UnquoteSplice(items) =>
            Left(new ExpectingCommaOrEndOfArray(arrayItems(items) :: ctors, returnState))
          case UnquoteOptional(item) =>
            Left(new ExpectingCommaOrEndOfArray(optionalArrayItem(item) :: ctors, returnState))
          case other =>
            new ExpectingDatum({ v => new ExpectingCommaOrEndOfArray(arrayItem(v) :: ctors, returnState) }, expecting).step(other)
        }
    }

    class ExpectingCommaOrEndOfArray(ctors: List[ArrayCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "comma or end of array") {
          case Token(TokenCloseBracket()) =>
            Left(returnState(finishArray(ctors)))
          case Token(TokenComma()) =>
            Left(new ExpectingArrayDatum(ctors, returnState, "datum"))
        }
    }

    class ExpectingFieldOrEndOfObject(ctors: List[ObjectCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case Token(TokenCloseBrace()) =>
            Left(returnState(finishObject(ctors)))
          case other =>
            new ExpectingFieldName(ctors, returnState, "field name or end of object").step(other)
        }
    }

    class ExpectingFieldName(ctors: List[ObjectCtor], returnState: Expr[JValue] => State, expecting: String = "field name") extends State {
      def step(thing: Tokenized) =
        withThing(thing, expecting) {
          case UnquoteSplice(items) =>
            Left(new ExpectingCommaOrEndOfObject(objectItems(items) :: ctors, returnState))
          case Unquote(s) =>
            Left(new ExpectingColon(encodeField(s), ctors, returnState))
          case Token(TokenString(s)) =>
            Left(new ExpectingColon(Expr(s), ctors, returnState))
          case Token(TokenIdentifier(s)) =>
            Left(new ExpectingColon(Expr(s), ctors, returnState))
        }
    }

    class ExpectingColon(field: Expr[String], ctors: List[ObjectCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "colon") {
          case Token(TokenColon()) =>
            Left(new ExpectingFieldValue(field, ctors, returnState))
        }
    }

    class ExpectingFieldValue(field: Expr[String], ctors: List[ObjectCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case UnquoteOptional(item) =>
            Left(new ExpectingCommaOrEndOfObject(optionalObjectItem(field, item) :: ctors, returnState))
          case other =>
            new ExpectingDatum({ v => new ExpectingCommaOrEndOfObject(objectItem(field, v) :: ctors, returnState) }).step(other)
        }
    }

    class ExpectingCommaOrEndOfObject(ctors: List[ObjectCtor], returnState: Expr[JValue] => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "comma or end of object") {
          case Token(TokenCloseBrace()) =>
            Left(returnState(finishObject(ctors)))
          case Token(TokenComma()) =>
            Left(new ExpectingFieldName(ctors, returnState))
        }
    }

    def walk(initialThings: List[Tokenized]): Expr[JValue] = {
      var things = initialThings
      var state: State = new TopLevelDatum
      while(!things.isEmpty) {
        state.step(things.head) match {
          case Right(result) =>
            return result
          case Left(newState) =>
            state = newState
        }
        things = things.tail
      }
      report.throwError("Internal error: Should have gotten to an end")
    }

    def move(pos: Position, offset: Int): Position = {
      Position(pos.sourceFile, pos.start + offset, pos.start + offset + 1)
    }
    def offsetOf(s: String, p: io.Position) = {
      val before = s.split("\n", -1).take(p.row - 1).map(_.length + 1).sum
      val result = before + p.column - 1
      result
    }
    def relativize(start: Position, s: String, j: io.Position): Position =
      if(j == io.Position.Invalid) move(start, s.length)
      else move(start, offsetOf(s, j))

    case class TokenInfo(tokens: List[JsonToken])(val nextUnquote: Expr[Any] => Tokenized, val orig: String, val origPos: Position) {
      def pop = TokenInfo(tokens.tail)(nextUnquote, orig, origPos)
      def position = tokens.headOption match {
        case Some(hd) => relativize(origPos, orig, hd.position)
        case None => relativize(origPos, orig, io.Position.Invalid)
      }
      def positionAtEnd = relativize(origPos, orig, io.Position(row = orig.split("\n", -1).length,
                                                                column = orig.length - orig.lastIndexOf('\n')))
    }

    def stripPos(msg: String): String = {
      val PfxRegex = """-?\d+:-?\d+: (.*)""".r
      msg match {
        case PfxRegex(rest) => rest
        case noPfx => noPfx
      }
    }

    def tokens(s: String, pos: Position) = try {
      new JsonTokenIterator(new StringReader(s)).toList
    } catch {
      case e: JsonLexException =>
       bail(stripPos(e.message), relativize(pos, s, e.position))
    }

    try {
      (strCtxExpr, piecesExpr) match {
        case ('{ StringContext(${Varargs(rawPartExprs)} : _*) }, Varargs(pieces)) =>
          val parts = rawPartExprs.map { rawPartExpr =>
            val rawPart = rawPartExpr.valueOrError;
            val (part, nextUnquote) =
              if(rawPart.endsWith("..")) (rawPart.dropRight(2), UnquoteSplice)
              else if(rawPart.endsWith("?")) (rawPart.dropRight(1), UnquoteOptional)
              else (rawPart, Unquote)
            try {
              TokenInfo(tokens(part, rawPartExpr.asTerm.pos))(nextUnquote, part, rawPartExpr.asTerm.pos)
            } catch {
              case e: JsonReaderException =>
                bail(stripPos(e.message), relativize(rawPartExpr.asTerm.pos, rawPart, e.position))
            }
          }

          if(parts.length != pieces.length + 1) bail("Using non-interpolator syntax for JSON is not supported", piecesExpr.asTerm.pos)

          val builder = List.newBuilder[Tokenized]
          def interweave(parts: List[TokenInfo], pieces: List[Expr[Any]]): Unit = {
            (parts, pieces) match {
              case (Nil, Nil) => // done
              case ((ti: TokenInfo) :: Nil, Nil) =>
                var remaining = ti
                while(!remaining.tokens.isEmpty) {
                  val left = remaining
                  builder += Token(remaining.tokens.head)(left.position)
                  remaining = remaining.pop
                }
                if(remaining.nextUnquote ne Unquote) {
                  builder += DeferredError(remaining.positionAtEnd, "Splice at end of input")
                }
                builder += End(ti.positionAtEnd)
              case ((ti: TokenInfo) :: rest, piece :: pieces) =>
                var remaining = ti
                while(!remaining.tokens.isEmpty) {
                  val left = remaining
                  builder += Token(remaining.tokens.head)(left.position)
                  remaining = remaining.pop
                }
                builder += remaining.nextUnquote(piece)
                interweave(rest, pieces)
              case _ =>
                bail("Internal error: pieces and parts didn't line up", piecesExpr.asTerm.pos)
            }
          }
          interweave(parts.toList, pieces.toList)

          val r = walk(builder.result())
          // println(r.show)
          r
        case _ =>
          bail("Using non-interpolator syntax for JSON is not supported", piecesExpr.asTerm.pos)
      }
    } catch {
      case Bail(msg, pos) =>
        report.error(msg, pos)
        '{ ??? : JValue }
    }
  }
}
