package unnamed_lang

import scala.collection.mutable

sealed trait Expr:
  def eval(env: Environment): Value

case class ExprList(v: List[Expr]) extends Expr {
  override def eval(env: Environment): Value = v match {
    case head :: args => head.eval(env) match {
      case ValueFn(fun) => fun(env, args)
      case x => throw NotCallableException(x)
    }
    case List() => throw EmptyExpressionException
  }
}

case class ExprIdentifier(v: String) extends Expr:
  override def eval(env: Environment): Value =
    env.get(v).getOrElse(throw NotDefinedException(v))

case class ExprPrimitive(v: Primitive) extends Expr:
  override def eval(env: Environment): Value = v


case class NotCallableException(value: Value) extends InterpretationException

object EmptyExpressionException extends InterpretationException

case class NotDefinedException(var_name: String) extends InterpretationException


case class Closure(captured_env: Environment, arg_names: List[String], body: Expr) {
  def apply(parent_env: Environment, args: List[Expr]): Value = {
    assert(args.length == arg_names.length)
    val environment = new Environment(Some(captured_env), mutable.Map() ++ arg_names.zip(args.map(_.eval(parent_env))))
    body.eval(environment)
  }
}
