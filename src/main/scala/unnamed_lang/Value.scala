package unnamed_lang

sealed trait Value
case class ValueFn(v: (Environment, List[Expr]) => Value) extends Value

sealed trait Primitive extends Value
case class ValueString(v: String) extends Primitive
case class ValueInt(v: Int) extends Primitive
case class ValueBool(v: Boolean) extends Primitive
