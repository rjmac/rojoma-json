package com.rojoma.json.v3
package `-impl`.interpolation

import java.io.StringReader

import ast._
import io._

import `-impl`.util.MacroCompat._

object JsonInterpolatorImpl {
  def j(c: Context)(pieces: c.Expr[Any]*): c.Expr[JValue] = {
    import c.universe._

    def stripPos(msg: String): String = {
      val PfxRegex = """-?\d+:-?\d+: (.*)""".r
      msg match {
        case PfxRegex(rest) => rest
        case noPfx => noPfx
      }
    }

    def move(pos: c.universe.Position, offset: Int): c.universe.Position =
      if(pos.isRange) pos.withStart(pos.start + offset)
      else pos.withPoint(pos.start + offset)

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

    case class TokenInfo(tokens: List[JsonToken])(orig: String, pos: c.universe.Position) {
      def pop = TokenInfo(tokens.tail)(orig, pos)
      def position = tokens.headOption match {
        case Some(hd) => relativize(pos, orig, hd.position)
        case None => relativize(pos, orig, Position.Invalid)
      }
    }
    type Tokens = List[TokenInfo]
    type Data = List[c.Expr[Any]]

    trait State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data)
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data)
    }

    def unexpectedDatum(data: Data, what: String, posIfNoData: c.universe.Position): Nothing = data match {
      case hd :: _ => c.abort(hd.tree.pos, s"Expected $what, got interpolated datum")
      case Nil => c.abort(posIfNoData, new JsonParserEOF(Position.Invalid).message)
    }

    case class ExpectingDatum(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(Nil)) :: tl =>
            if(data.isEmpty) c.abort(ti.position, new JsonParserEOF(Position.Invalid).message)
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.codec.JsonEncode.toJValue(${data.head})", tl, data.tail)
          case (ti@TokenInfo(TokenOpenBrace() :: _)) :: tl2 =>
            (Right(ExpectingFieldOrEndOfObject(parentState)), ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenOpenBracket() :: _)) :: tl2 =>
            (Right(ExpectingElementOrEndOfArray(parentState)), ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenIdentifier("true") :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalTrue", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenIdentifier("false") :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalFalse", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenIdentifier("null") :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JNull", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenString(s) :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JString($s)", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenNumber(n) :: _)) :: tl2 =>
            // Hmm.   We can inspect "n" and generate a more specific JNumber type.
            // But it's most likely we'll just be serializing the result, so there's
            // not much point.
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JNumber.unsafeFromString($n)", ti.pop :: tl2, data)
          case (ti@TokenInfo(t :: _)) :: _ =>
            c.abort(ti.position, stripPos(new JsonUnexpectedToken(t, "datum").message))
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        parentState.substateCompleted(datum, tokens, data)
    }

    case class ExpectingElementOrEndOfArray(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(TokenCloseBracket() :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JArray.canonicalEmpty", ti.pop :: tl2, data)
          case _ =>
            (Right(ExpectingDatum(this)), tokens, data)
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        (Right(ExpectingCommaOrEndOfArray(List(value), parentState)), tokens, data)
    }

    case class ExpectingCommaOrEndOfArray(elemsRev: List[Tree], parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(TokenCloseBracket() :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JArray(_root_.scala.collection.immutable.Vector(..${elemsRev.reverse}))", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenComma() :: _)) :: tl2 =>
            (Right(ExpectingDatum(this)), ti.pop :: tl2, data)
          case (ti@TokenInfo(t :: _)) :: _ =>
            c.abort(ti.position, stripPos(new JsonUnexpectedToken(t, "comma or end of array").message))
          case (ti@TokenInfo(Nil)) :: _ =>
            unexpectedDatum(data, "comma or end of array", ti.position)
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        (Right(ExpectingCommaOrEndOfArray(value :: elemsRev, parentState)), tokens, data)
    }

    case class ExpectingFieldOrEndOfObject(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(TokenCloseBrace() :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JObject.canonicalEmpty", ti.pop :: tl2, data)
          case _ =>
            (Right(ExpectingField(ExpectingCommaOrEndOfObject(Nil, parentState))), tokens, data)
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        abort("Internal error: this never delegates to a substate")
    }

    case class ExpectingField(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(Nil)) :: tl =>
            if(data.isEmpty) c.abort(ti.position, new JsonParserEOF(Position.Invalid).message)
            val field = q"${data.head} : _root_.scala.Predef.String"
            (Right(ExpectingColon(field, parentState)), tl, data.tail)
          case (ti@TokenInfo(TokenIdentifier(s) :: _)) :: tl2 =>
            (Right(ExpectingColon(q"$s", parentState)), ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenString(s) :: _)) :: tl2 =>
            (Right(ExpectingColon(q"$s", parentState)), ti.pop :: tl2, data)
          case (ti@TokenInfo(t :: _)) :: _ =>
            c.abort(ti.position, stripPos(new JsonUnexpectedToken(t, "field name").message))
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        abort("Internal error: this never delegates to a substate")
    }

    case class ExpectingColon(field: Tree, parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(TokenColon() :: _)) :: tl2 =>
            (Right(ExpectingDatum(this)), ti.pop :: tl2, data)
          case (ti@TokenInfo(t :: _)) :: _ =>
            c.abort(ti.position, stripPos(new JsonUnexpectedToken(t, "colon").message))
          case (ti@TokenInfo(Nil)) :: _ =>
            unexpectedDatum(data, "colon", ti.position)
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        parentState.substateCompleted(q"_root_.scala.Tuple2($field, $value)", tokens, data)
    }

    case class ExpectingCommaOrEndOfObject(fieldsRev: List[Tree], parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (ti@TokenInfo(TokenCloseBrace() :: _)) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JObject(_root_.scala.collection.immutable.Map(..${fieldsRev.reverse}))", ti.pop :: tl2, data)
          case (ti@TokenInfo(TokenComma() :: _)) :: tl2 =>
            (Right(ExpectingField(this)), ti.pop :: tl2, data)
          case (ti@TokenInfo(t :: _)) :: _ =>
            c.abort(ti.position, stripPos(new JsonUnexpectedToken(t, "comma or end of object").message))
          case (ti@TokenInfo(Nil)) :: _ =>
            unexpectedDatum(data, "comma or end of object", ti.position)
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        (Right(ExpectingCommaOrEndOfObject(value :: fieldsRev, parentState)), tokens, data)
    }

    object RootState extends State {
      def next(tokens: Tokens, data: Data) = (Right(ExpectingDatum(this)), tokens, data)
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Left[Tree, Nothing], Tokens, Data) =
        (Left(value), tokens, data)
    }

    def walk(parts: Tokens, pieces: Data): Tree = {
      def leftoverData = "Leftover data in JSON literal"
      def loop(state: State, parts: Tokens, pieces: Data): Tree = {
        state.next(parts, pieces) match {
          case (Right(nextState), parts2, pieces2) =>
            loop(nextState, parts2, pieces2)
          case (Left(tree), TokenInfo(Nil) :: Nil, Nil) =>
            tree
          case (Left(tree), Nil, Nil) =>
            tree
          // I don't think most of these cases can happen, but let's
          // keep the exhaustivity checker happy.
          case (Left(_), (ti@TokenInfo(_ :: _)) :: _, _) =>
            c.abort(ti.position, leftoverData)
          case (Left(_), TokenInfo(Nil) :: ((ti@TokenInfo(_ :: _)) :: tl), _) =>
            c.abort(ti.position, leftoverData)
          case (Left(_), _, hd :: _) =>
            c.abort(hd.tree.pos, leftoverData)
          case (Left(_), xs, Nil) =>
            xs.find(_.tokens.nonEmpty) match {
              case Some(ti) =>
                c.abort(ti.position, leftoverData)
               case None =>
                abort(leftoverData)
            }
        }
      }
      loop(RootState, parts, pieces)
    }

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        val parts = rawParts map {
          case node@Literal(Constant(part: String)) =>
            try {
              TokenInfo(tokens(part, node.pos))(part, node.pos)
            } catch {
              case e: JsonReaderException =>
                c.abort(relativize(node.pos, part, e.position), stripPos(e.message))
            }
        }
        if(parts.length != pieces.length + 1) abort("Internal error: there is not one more piece than part")

        val tree = walk(parts.toList, pieces.toList)
        // println(tree)
        c.Expr[JValue](tree)
      case _ =>
        abort("Not called from interpolation position.")
    }
  }
}
