package unnamed_lang

import scala.collection.mutable

class Environment
(
  parent: Option[Environment] = Some(DefaultEnvironment),
  map: mutable.Map[String, Value] = mutable.Map()
) {
  def define(ident: String, v: Value): Unit =
    map.put(ident, v)

  def find(ident: String, v: Value): Option[Environment] =
    if (map.contains(ident))
      Some(this)
    else
      parent.flatMap(_.find(ident, v))

  def get(ident: String): Option[Value] =
    map.get(ident).orElse(parent.flatMap(_.get(ident)))
}

object DefaultEnvironment extends Environment(parent = None, map = mutable.Map().addAll(Seq(
  "+" -> ValueFn((env, list) => list.map(_.eval(env)) match {
    case ValueInt(v1) :: ValueInt(v2) :: List() => ValueInt(v1 + v2)
    case _ => throw InvalidArgumentException("+", "2 ints")
  }),
  "-" -> ValueFn((env, list) => list.map(_.eval(env)) match {
    case ValueInt(v1) :: ValueInt(v2) :: List() => ValueInt(v1 - v2)
    case ValueInt(v) :: List() => ValueInt(- v)
    case _ => throw InvalidArgumentException("-", "1 or 2 ints")
  }),
  "*" -> ValueFn((env, list) => list.map(_.eval(env)) match {
    case ValueInt(v1) :: ValueInt(v2) :: List() => ValueInt(v1 * v2)
    case _ => throw InvalidArgumentException("*", "2 ints")
  }),
  "print" -> ValueFn((env, list) => list match {
    case expr :: List() =>
      val v = expr.eval(env)
      print(v)
      v
    case _ => throw InvalidArgumentException("print", "1 value")
  }),
  "println" -> ValueFn((env, list) => list match {
    case expr :: List() =>
      val v = expr.eval(env)
      println(v)
      v
    case _ => throw InvalidArgumentException("println", "1 value")
  }),
  "scope" -> ValueFn((parent_env, exprs) => {
    val env = new Environment(Some(parent_env))
    exprs.map(e => e.eval(env)).lastOption.getOrElse(throw InvalidArgumentException("scope", "at least 1 expression"))
  }),
  "def" -> ValueFn((parent_env, exprs) => exprs match {
    case ExprIdentifier(ident) :: expr :: List() =>
      val v = expr.eval(parent_env)
      parent_env.define(ident, v)
      v
    case _ => throw InvalidArgumentException("def", "identifier and a value")
  }),
  "lambda" -> ValueFn((parent_env, exprs) => exprs match {
    case ExprList(args) :: body :: List() =>
      val arg_names = args.map({
        case ExprIdentifier(ident) => ident;
        case _ => throw InvalidArgumentException("lambda", "first argument to be list of identifiers")
      })
      ValueFn(Closure(parent_env, arg_names, body).apply)
    case _ => throw throw InvalidArgumentException("lambda", "list of identifiers and expression")
  }),
  "<" -> ValueFn((parent_env, args) => args.map(_.eval(parent_env)) match {
    case ValueInt(v1) :: ValueInt(v2) :: List() => ValueBool(v1 < v2)
    case _ => throw InvalidArgumentException("<", "2 ints")
  }),
  "==" -> ValueFn((parent_env, args) => args.map(_.eval(parent_env)) match {
    case v1 :: v2 :: List() => ValueBool(v1 == v2)
    case _ => throw InvalidArgumentException("==", "2 ints")
  }),
  "if" -> ValueFn((parent_env, args) => args match {
    case cond :: ifThen :: ifElse :: List() =>
      cond.eval(parent_env) match {
        case ValueBool(true) => ifThen.eval(parent_env)
        case ValueBool(false) => ifElse.eval(parent_env)
        case _ => throw InvalidArgumentException("if", "condition to evaluate to bool")
      }
    case _ => throw InvalidArgumentException("if", "bool and 2 expressions")
  },
))))

case class InvalidArgumentException(function: String, expects: String) extends InterpretationException
