package unnamed_lang

import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  def assertEvalsTo(code: String, expected: Value): Unit = {
    val res = (new Interpreter).eval(code)
    assert(res == expected)
  }

  test("simple math") {
    assertEvalsTo("(+ (* 2 3) 4)", ValueInt(10))
  }
  test("returns last") {
    assertEvalsTo("0 (+ 1 2) 3", ValueInt(3))
  }
  test("simple lambda") {
    assertEvalsTo("((lambda () 42))", ValueInt(42))
  }
  test("binding variables") {
    assertEvalsTo("(def x 42) x", ValueInt(42))
  }
  test("recursion") {
    assertEvalsTo(
      "(def factorial (lambda (n) (if (< 0 n) (* n (factorial (- n 1))) 1))) (factorial 5)",
      ValueInt(120))
  }
  test("cons cells") {
    assertEvalsTo(
      """
        |(def cons (lambda (a b) (lambda (f) (f a b))))
        |(def car (lambda (pair) (pair (lambda (a b) a))))
        |(def cdr (lambda (pair) (pair (lambda (a b) b))))
        |(car (cdr (cons 1 (cons 2 3))))
        |""".stripMargin,
        ValueInt(2))
  }
}
