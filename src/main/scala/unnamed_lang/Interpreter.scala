package unnamed_lang

import unnamed_lang.parsing.Parser

class Interpreter {
  val environment = new Environment()

  def eval(expr: Expr): Value = expr.eval(environment)
  def eval(code: String): Value = eval(Parser(code))
}

trait InterpretationException extends RuntimeException
