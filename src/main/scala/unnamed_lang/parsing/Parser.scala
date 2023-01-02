package unnamed_lang.parsing

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

import unnamed_lang._

object Parser extends Parsers {
  override type Elem = Token

  class SeqReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Token] = new SeqReader(tokens.tail)
  }

  def vec: Parser[ExprList] =
    (LEFT_PAREN ~ rep(expr) ~ RIGHT_PAREN) ^^ { case _ ~ list ~ _ => ExprList(list) }

  def primitive: Parser[ExprPrimitive] =
    accept("primitive", { case TPrimitive(primitive) => ExprPrimitive(primitive) })

  def identifier: Parser[ExprIdentifier] =
    accept("identifier", { case TIdentifier(ident) => ExprIdentifier(ident) })

  def expr: Parser[Expr] =
    vec | primitive | identifier

  def program: Parser[Expr] =
    rep(expr) ^^ { list => ExprList(ExprIdentifier("scope") :: list) }

  def apply(source: String): Expr = program(new SeqReader(Scanner(source))).get
}
