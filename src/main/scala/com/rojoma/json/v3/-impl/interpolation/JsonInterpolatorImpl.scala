package com.rojoma.json.v3
package `-impl`.interpolation

import java.io.StringReader

import ast._
import io._

import `-impl`.util.MacroCompat
import `-impl`.util.MacroCompat._

object JsonInterpolatorImpl {
  def j(c0: Context)(pieces: c0.Expr[Any]*): c0.Expr[JValue] = {
    val mc = new MacroCompat {
      val c : c0.type = c0
    }
    import mc._
    import c.universe._

    def freshTermName(): TermName = toTermName(c.freshName())

    sealed abstract class Tokenized {
      def position: c.universe.Position
    }
    case class Token(token: JsonToken)(pos: => c.universe.Position) extends Tokenized {
      lazy val position = pos
    }
    case class Unquote(tree: Tree) extends Tokenized {
      def position = tree.pos
    }
    case class UnquoteSplice(tree: Tree) extends Tokenized {
      def position = tree.pos
    }
    case class UnquoteOptional(tree: Tree) extends Tokenized {
      def position = tree.pos
    }
    case class DeferredError(position: c.universe.Position, message: String) extends Tokenized
    case class End(position: c.universe.Position) extends Tokenized

    def stripPos(msg: String): String = {
      val PfxRegex = """-?\d+:-?\d+: (.*)""".r
      msg match {
        case PfxRegex(rest) => rest
        case noPfx => noPfx
      }
    }

    def move(pos: c.universe.Position, offset: Int): c.universe.Position =
      if(pos.isRange) pos.withStart(pos.start + offset)
      else pos.withPoint(pos.point + offset)

    def offsetOf(s: String, p: io.Position) = {
      val before = s.split("\n", -1).take(p.row - 1).map(_.length + 1).sum
      val result = before + p.column - 1
      result
    }
    def relativize(start: c.universe.Position, s: String, j: io.Position): c.universe.Position =
      if(j == io.Position.Invalid) move(start, s.length)
      else move(start, offsetOf(s, j))

    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
    def tokens(s: String, pos: c.universe.Position) = try {
      new JsonTokenIterator(new StringReader(s)).toList
    } catch {
      case e: JsonLexException =>
       c.abort(relativize(pos, s, e.position), stripPos(e.message))
    }

    case class TokenInfo(tokens: List[JsonToken])(val nextUnquote: Tree => Tokenized, val orig: String, val origPos: c.universe.Position) {
      def pop = TokenInfo(tokens.tail)(nextUnquote, orig, origPos)
      def position = tokens.headOption match {
        case Some(hd) => relativize(origPos, orig, hd.position)
        case None => relativize(origPos, orig, Position.Invalid)
      }
      def positionAtEnd = relativize(origPos, orig, io.Position(row = orig.length - (orig.lastIndexOf('\n') + 1),
                                                                column = orig.split("\n", -1).length - 1))
    }

    sealed trait State {
      def step(thing: Tokenized): Either[State, Tree]
    }

    def finishArray(ctors: List[TermName => Tree]): Tree = {
      if(ctors.isEmpty) q"_root_.com.rojoma.json.v3.ast.JArray.canonicalEmpty"
      else {
        val temp = freshTermName()
        val populations = ctors.reverse.map(_(temp))
        q"""{
          val $temp = _root_.scala.collection.immutable.Vector.newBuilder[_root_.com.rojoma.json.v3.ast.JValue]
          ..$populations
          _root_.com.rojoma.json.v3.ast.JArray($temp.result())
        }"""
      }
    }

    def finishObject(ctors: List[TermName => Tree]): Tree = {
      if(ctors.isEmpty) q"_root_.com.rojoma.json.v3.ast.JObject.canonicalEmpty"
      else {
        val temp = freshTermName()
        val populations = ctors.reverse.map(_(temp))
        q"""{
          val $temp = _root_.scala.collection.immutable.ListMap.newBuilder[_root_.java.lang.String, _root_.com.rojoma.json.v3.ast.JValue]
          ..$populations
          _root_.com.rojoma.json.v3.ast.JObject($temp.result())
        }"""
      }
    }

    def arrayItem(v: Tree): TermName => Tree = { termName =>
      q"$termName += $v"
    }

    def optionalArrayItem(v: Tree): TermName => Tree = { termName =>
      q"$termName ++= _root_.com.rojoma.json.v3.`-impl`.interpolation.Convert.option($v)"
    }

    def arrayItems(v: Tree): TermName => Tree = { termName =>
      q"$termName ++= _root_.com.rojoma.json.v3.`-impl`.interpolation.Convert.array($v)"
    }

    def objectItem(k: Tree, v: Tree): TermName => Tree = { termName =>
      q"$termName += (($k, $v))"
    }

    def optionalObjectItem(k: Tree, v: Tree): TermName => Tree = { termName =>
      val kTemp = freshTermName
      val temp = freshTermName()
      q"""{
            val $kTemp = $k
            _root_.com.rojoma.json.v3.`-impl`.interpolation.Convert.option($v) match {
              case _root_.scala.Some($temp) => $termName += (($kTemp, $temp))
              case _root_.scala.None => {}
            }
          }"""
    }

    def objectItems(v: Tree): TermName => Tree = { termName =>
      q"""$termName ++= _root_.com.rojoma.json.v3.`-impl`.interpolation.Convert.map($v)"""
    }

    def unexpectedTokenized(thing: Tokenized, expecting: String): Nothing =
      thing match {
        case Token(t) =>
          c.abort(thing.position, stripPos(new JsonUnexpectedToken(t, expecting).message))
        case DeferredError(pos, what) =>
          c.abort(pos, what)
        case End(pos) =>
          c.abort(pos, new JsonParserEOF(Position.Invalid).message)
        case UnquoteSplice(t) =>
          c.abort(thing.position, s"Expected $expecting; got splicing unquote")
        case UnquoteOptional(t) =>
          c.abort(thing.position, s"Expected $expecting; got optional unquote")
        case Unquote(t) =>
          c.abort(thing.position, s"Expected $expecting; got unquote")
      }

    def withThing(thing: Tokenized, expecting: String)(f: PartialFunction[Tokenized, Either[State, Tree]]): Either[State, Tree] =
      f.applyOrElse(thing, unexpectedTokenized(_ : Tokenized, expecting))

    class ExpectingArrayDatum(ctors: List[TermName => Tree], returnState: Tree => State, expecting: String) extends State {
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


    class ExpectingCommaOrEndOfArray(ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "comma or end of array") {
          case Token(TokenCloseBracket()) =>
            Left(returnState(finishArray(ctors)))
          case Token(TokenComma()) =>
            Left(new ExpectingArrayDatum(ctors, returnState, "datum"))
        }
    }

    class ExpectingDatumOrEndOfArray(ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case Token(TokenCloseBracket()) =>
            Left(returnState(finishArray(ctors)))
          case other =>
            new ExpectingArrayDatum(ctors, returnState, "datum or end of array").step(other)
        }
    }

    class ExpectingCommaOrEndOfObject(ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "comma or end of object") {
          case Token(TokenCloseBrace()) =>
            Left(returnState(finishObject(ctors)))
          case Token(TokenComma()) =>
            Left(new ExpectingFieldName(ctors, returnState))
        }
    }

    class ExpectingFieldValue(field: Tree, ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case UnquoteOptional(item) =>
            Left(new ExpectingCommaOrEndOfObject(optionalObjectItem(field, item) :: ctors, returnState))
          case other =>
            new ExpectingDatum({ v => new ExpectingCommaOrEndOfObject(objectItem(field, v) :: ctors, returnState) }).step(other)
        }
    }

    class ExpectingColon(field: Tree, ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "colon") {
          case Token(TokenColon()) =>
            Left(new ExpectingFieldValue(field, ctors, returnState))
        }
    }

    class ExpectingFieldName(ctors: List[TermName => Tree], returnState: Tree => State, expecting: String = "field name") extends State {
      def step(thing: Tokenized) =
        withThing(thing, expecting) {
          case UnquoteSplice(items) =>
            Left(new ExpectingCommaOrEndOfObject(objectItems(items) :: ctors, returnState))
          case Unquote(s) =>
            Left(new ExpectingColon(q"_root_.com.rojoma.json.v3.codec.FieldEncode.toField($s)", ctors, returnState))
          case Token(TokenString(s)) =>
            Left(new ExpectingColon(q"$s", ctors, returnState))
          case Token(TokenIdentifier(s)) =>
            Left(new ExpectingColon(q"$s", ctors, returnState))
        }
    }

    class ExpectingFieldOrEndOfObject(ctors: List[TermName => Tree], returnState: Tree => State) extends State {
      def step(thing: Tokenized) =
        thing match {
          case Token(TokenCloseBrace()) =>
            Left(returnState(finishObject(ctors)))
          case other =>
            new ExpectingFieldName(ctors, returnState, "field name or end of object").step(other)
        }
    }

    class ExpectingDatum(returnState: Tree => State, expecting: String = "datum") extends State {
      def step(thing: Tokenized) =
        withThing(thing, expecting) {
          case Unquote(item) =>
            Left(returnState(q"_root_.com.rojoma.json.v3.codec.JsonEncode.toJValue($item)"))
          case Token(TokenOpenBracket()) =>
            Left(new ExpectingDatumOrEndOfArray(Nil, returnState))
          case Token(TokenOpenBrace()) =>
            Left(new ExpectingFieldOrEndOfObject(Nil, returnState))
          case Token(TokenString(s)) =>
            Left(returnState(q"_root_.com.rojoma.json.v3.ast.JString($s)"))
          case Token(TokenIdentifier("true")) =>
            Left(returnState(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalTrue"))
          case Token(TokenIdentifier("false")) =>
            Left(returnState(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalFalse"))
          case Token(TokenIdentifier("null")) =>
            Left(returnState(q"_root_.com.rojoma.json.v3.ast.JNull"))
          case Token(TokenNumber(n)) =>
            // Hmm.   We can inspect "n" and generate a more specific JNumber type.
            // But it's most likely we'll just be serializing the result, so there's
            // not much point.
            Left(returnState(q"_root_.com.rojoma.json.v3.ast.JNumber.unsafeFromString($n)"))
        }
    }

    class ExpectingEnd(tree: Tree) extends State {
      def step(thing: Tokenized) =
        withThing(thing, "end of input") {
          case End(_) => Right(tree)
        }
    }

    class TopLevelDatum extends State {
      def step(thing: Tokenized) =
        new ExpectingDatum(new ExpectingEnd(_)).step(thing)
    }


    def walk(initialThings: List[Tokenized]): Tree = {
      var things = initialThings
      var state: State = new TopLevelDatum
      while(!things.isEmpty) {
        state.step(things.head) match {
          case Right(tree) =>
            return tree
          case Left(newState) =>
            state = newState
        }
        things = things.tail
      }
      abort("Internal error: Should have gotten to an end")
    }

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        val parts = rawParts map {
          case node@Literal(Constant(rawPart: String)) =>
            val (part, nextUnquote) =
              if(rawPart.endsWith("..")) (rawPart.dropRight(2), UnquoteSplice)
              else if(rawPart.endsWith("?")) (rawPart.dropRight(1), UnquoteOptional)
              else (rawPart, Unquote)
            try {
              TokenInfo(tokens(part, node.pos))(nextUnquote, part, node.pos)
            } catch {
              case e: JsonReaderException =>
                c.abort(relativize(node.pos, part, e.position), stripPos(e.message))
            }
        }
        if(parts.length != pieces.length + 1) abort("Internal error: there is not one more piece than part")

        val builder = List.newBuilder[Tokenized]
        def interweave(parts: List[TokenInfo], pieces: List[c.Expr[Any]]): Unit = {
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
              builder += remaining.nextUnquote(piece.tree)
              interweave(rest, pieces)
            case _ =>
              abort("Internal error: pieces and parts didn't line up")
          }
        }
        interweave(parts.toList, pieces.toList)

        val tree = walk(builder.result())
        //println(tree)
        c.Expr[JValue](tree)
      case _ =>
        abort("Not called from interpolation position.")
    }
  }
}
