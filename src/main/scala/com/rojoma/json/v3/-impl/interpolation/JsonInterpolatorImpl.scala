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

    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
    def tokens(s: String, pos: Position) = try {
      new JsonTokenIterator(new StringReader(s)).toList.map(_.unpositioned)
    } catch {
      case e: JsonLexException =>
       c.abort(pos, stripPos(e.message))
    }

    type Tokens = List[(List[JsonToken], Position)]
    type Data = List[c.Expr[Any]]

    trait State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data)
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data)
    }

    def unexpectedDatum(data: Data, what: String): Nothing = data match {
      case hd :: _ => c.abort(hd.tree.pos, s"Expected $what, got interpolated datum")
      case Nil => abort("Internal error: No more tokens, no more data?")
    }

    case class ExpectingDatum(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (Nil, pos) :: tl =>
            if(data.isEmpty) c.abort(pos,"Internal error: no more data")
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.codec.JsonEncode.toJValue(${data.head})", tl, data.tail)
          case (TokenOpenBrace() :: tl1, pos) :: tl2 =>
            (Right(ExpectingFieldOrEndOfObject(parentState)), (tl1,pos) :: tl2, data)
          case (TokenOpenBracket() :: tl1, pos) :: tl2 =>
            (Right(ExpectingElementOrEndOfArray(parentState)), (tl1,pos) :: tl2, data)
          case (TokenIdentifier("true") :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalTrue", (tl1,pos) :: tl2, data)
          case (TokenIdentifier("false") :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JBoolean.canonicalFalse", (tl1,pos) :: tl2, data)
          case (TokenIdentifier("null") :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JNull", (tl1,pos) :: tl2, data)
          case (TokenString(s) :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JString($s)", (tl1,pos) :: tl2, data)
          case (TokenNumber(n) :: tl1, pos) :: tl2 =>
            // Hmm.   We can inspect "n" and generate a more specific JNumber type.
            // But it's most likely we'll just be serializing the result, so there's
            // not much point.
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JNumber.unsafeFromString($n)", (tl1,pos) :: tl2, data)
          case (t :: _, pos) :: _ =>
            c.abort(pos, new JsonUnexpectedToken(t, "datum").message)
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        parentState.substateCompleted(datum, tokens, data)
    }

    case class ExpectingElementOrEndOfArray(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (TokenCloseBracket() :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JArray.canonicalEmpty", (tl1,pos) :: tl2, data)
          case _ =>
            (Right(ExpectingDatum(this)), tokens, data)
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        (Right(ExpectingCommaOrEndOfArray(List(value), parentState)), tokens, data)
    }

    case class ExpectingCommaOrEndOfArray(elemsRev: List[Tree], parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (TokenCloseBracket() :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JArray(_root_.scala.collection.immutable.Vector(..${elemsRev.reverse}))", (tl1, pos) :: tl2, data)
          case (TokenComma() :: tl1, pos) :: tl2 =>
            (Right(ExpectingDatum(this)), (tl1, pos) :: tl2, data)
          case (t :: _, pos) :: _ =>
            c.abort(pos, new JsonUnexpectedToken(t, "comma or end of array").message)
          case (Nil, _) :: _ =>
            unexpectedDatum(data, "comma or end of array")
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        (Right(ExpectingCommaOrEndOfArray(value :: elemsRev, parentState)), tokens, data)
    }

    case class ExpectingFieldOrEndOfObject(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (TokenCloseBrace() :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JObject.canonicalEmpty", (tl1,pos) :: tl2, data)
          case _ =>
            (Right(ExpectingField(ExpectingCommaOrEndOfObject(Nil, parentState))), tokens, data)
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        abort("Internal error: this never delegates to a substate")
    }

    case class ExpectingField(parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (Nil, pos) :: tl =>
            if(data.isEmpty) c.abort(pos, "Internal error: no more data")
            val field = q"${data.head} : _root_.scala.Predef.String"
            (Right(ExpectingColon(field, parentState)), tl, data.tail)
          case (TokenIdentifier(s) :: tl1, pos) :: tl2 =>
            (Right(ExpectingColon(q"$s", parentState)), (tl1,pos) :: tl2, data)
          case (TokenString(s) :: tl1, pos) :: tl2 =>
            (Right(ExpectingColon(q"$s", parentState)), (tl1,pos) :: tl2, data)
          case (t :: _, pos) :: _ =>
            c.abort(pos, new JsonUnexpectedToken(t, "field name").message)
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(datum: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        abort("Internal error: this never delegates to a substate")
    }

    case class ExpectingColon(field: Tree, parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (TokenColon() :: tl1, pos) :: tl2 =>
            (Right(ExpectingDatum(this)), (tl1, pos) :: tl2, data)
          case (t :: _, pos) :: _ =>
            c.abort(pos, new JsonUnexpectedToken(t, "colon").message)
          case (Nil, _) :: _ =>
            unexpectedDatum(data, "colon")
          case Nil =>
            abort("Internal error: no more tokens")
        }
      def substateCompleted(value: Tree, tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        parentState.substateCompleted(q"_root_.scala.Tuple2($field, $value)", tokens, data)
    }

    case class ExpectingCommaOrEndOfObject(fieldsRev: List[Tree], parentState: State) extends State {
      def next(tokens: Tokens, data: Data): (Either[Tree, State], Tokens, Data) =
        tokens match {
          case (TokenCloseBrace() :: tl1, pos) :: tl2 =>
            parentState.substateCompleted(q"_root_.com.rojoma.json.v3.ast.JObject(_root_.scala.collection.immutable.Map(..${fieldsRev.reverse}))", (tl1, pos) :: tl2, data)
          case (TokenComma() :: tl1, pos) :: tl2 =>
            (Right(ExpectingField(this)), (tl1, pos) :: tl2, data)
          case (t :: _, pos) :: _ =>
            c.abort(pos, new JsonUnexpectedToken(t, "comma or end of object").message)
          case (Nil, _) :: _ =>
            unexpectedDatum(data, "comma or end of object")
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
          case (Left(tree), (Nil, _) :: Nil, Nil) =>
            tree
          case (Left(tree), Nil, Nil) =>
            tree
          // I don't think most of these cases can happen, but let's
          // keep the exhaustivity checker happy.
          case (Left(_), (_ :: _, pos) :: _, _) =>
            c.abort(pos, leftoverData)
          case (Left(_), (Nil, _) :: ((_ :: _, pos) :: tl), _) =>
            c.abort(pos, leftoverData)
          case (Left(_), _, hd :: _) =>
            c.abort(hd.tree.pos, leftoverData)
          case (Left(_), xs, Nil) =>
            xs.find(_._1.nonEmpty) match {
              case Some((_, pos)) =>
                c.abort(pos, leftoverData)
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
              (tokens(part, node.pos), node.pos)
            } catch {
              case e: JsonReaderException =>
                c.abort(node.pos, e.message)
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
