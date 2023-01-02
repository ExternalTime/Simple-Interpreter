package unnamed_lang.parsing

import unnamed_lang._

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers

sealed trait Token
object LEFT_PAREN extends Token
object RIGHT_PAREN extends Token
case class TIdentifier(v: String) extends Token
case class TPrimitive[T <: Primitive](v: Primitive) extends Token

object Scanner extends RegexParsers {
  override def skipWhitespace: Boolean = true

  implicit def wrap_primitive[T <: Primitive](v: T): TPrimitive[T] =
    parsing.TPrimitive(v)

  def left_paren: Parser[LEFT_PAREN.type] = "(" ^^ { _ => LEFT_PAREN }
  def right_paren: Parser[RIGHT_PAREN.type] = ")" ^^ { _ => RIGHT_PAREN }
  def string_literal: Parser[TPrimitive[ValueString]] = """"[^"]*"""".r ^^ { str => ValueString(str.substring(1, str.length - 1)) }
  def i32: Parser[TPrimitive[ValueInt]] = """[+-]?[0-9]+""".r ^^ { int => ValueInt(int.toInt) }
  def bool: Parser[TPrimitive[ValueBool]] = "true|false".r ^^ { bool => ValueBool(bool.toBoolean) }
  def ident: Parser[TIdentifier] = """[^\s()]+""".r ^^ { TIdentifier.apply }

  def primitive: Parser[Token] =
    string_literal | i32 | bool

  def token: Parser[Token] =
    left_paren | right_paren | primitive | ident

  def tokens: Parser[List[Token]] =
    rep(token)

  def apply(source: String): List[Token] =
    Scanner.parse(Scanner.tokens, source).get
}
