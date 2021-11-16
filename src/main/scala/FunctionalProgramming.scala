package org.scala.learn

import scala.annotation.tailrec

object FunctionalProgramming extends App {

  val getMultiplier = (number: Int) => (n: Int) => n * number

  private val multiplier: Int => Int = getMultiplier(2)
  println(multiplier(3))
  println(multiplier(4))
  println(multiplier(5))
  println(getMultiplier(5)(4))

  val concat = (s1: String, s2: String) => s"$s1 $s2"
  println(concat("Hi", "there"))

  private val adder: Int => Int = (x: Int) => x + 1

  @tailrec
  def superAdder(adder: Int => Int, times: Int, value: Int): Int =
    if (times <= 0) value
    else superAdder(adder, times - 1, adder(value))

  println(superAdder(adder, 10, 0))

  def adderFunction(adder: Int => Int, times: Int): Int => Int =
    if (times <= 0) (x: Int) => x
    else (x: Int) => adderFunction(adder, times - 1)(adder(x))

  val function = adderFunction(adder, 20)
  println(function(0))

  def curriedFormatter(formatter: String)(value: Double): String = formatter.format(value)
  def superCurriedFunction(adder1: Int)(adder2: Int)(value: Int): Int = value + adder1 + adder2

  println(curriedFormatter("%.3f")(2.456))
  val standardFormatter: Double => String = curriedFormatter("%2.2f")
  println(curriedFormatter("%.3f")(2.456))
  println(standardFormatter(24123.4123214))

  private val superAdderFun: Int => Int = superCurriedFunction(1)(2)
  println(superAdderFun(3))

  def toCurry[S, T, U](fun: (S, T) => U): S => T => U =
    arg1 => arg2 => fun(arg1, arg2)

  def fromCurry[S, T, U](fun: S => T => U): (S, T) => U =
    (arg1, arg2) => fun(arg1)(arg2)

  val simpleSuperAdderFun = fromCurry(superCurriedFunction)
  println(simpleSuperAdderFun(3, 4)(5))

  val curriedSuperAdderFun = toCurry(simpleSuperAdderFun)
  println(curriedSuperAdderFun(3)(4)(5))

  def compose[T, S, U](fun1: T => S, fun2: U => T): U => S = x => fun1(fun2(x))
  def andThen[T, S, U](fun1: T => S, fun2: S => U): T => U = x => fun2(fun1(x))
  val adderFun = (x: Int) => x + 1
  val multiplierFun = (x: Int) => x * 2
  val doubleThenIncrementFunction = compose(adderFun, multiplierFun)
  val incrementThenDoubleFunction = andThen(adderFun, multiplierFun)
  println(doubleThenIncrementFunction(2))
  println(incrementThenDoubleFunction(3))

   // Advanced course
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val addSeven1 = (y: Int) => simpleAddFunction(7, y)
  val addSeven2 = (y: Int) => simpleAddMethod(7, y)
  val addSeven3 = (y: Int) => curriedAddMethod(7)(y)
  val addSeven4 = curriedAddMethod(7) _
  val addSeven5: Int => Int = curriedAddMethod(7)
  val addSeven6 = simpleAddFunction.curried(7)
  val addSeven7 = simpleAddFunction.curried(7)
  val addSeven8 = curriedAddMethod(7)(_)
  val addSeven9 = simpleAddFunction(7, _)
  val addSeven10 = simpleAddMethod(7, _: Int)

  val intro = (str1: String, str2: String, str3: String) => s"$str1 $str2 $str3"
  val introTemplate = intro("Hi! I'm", _, "and I'm learning scala")
  println(introTemplate("Ahmed"))

  val printFormatterNumbers = (numbers: Seq[Double], formatter: String) => numbers map(formatter.format(_)) foreach println

  val numbersPrinter = (numbers: Seq[Double]) => printFormatterNumbers(numbers, _)
  val numbers = Seq(4324.23423, 123.412, 12312.4123)
  val seqPrinter = numbersPrinter(numbers)
  seqPrinter("%.2f")
  seqPrinter("%.3f")
  seqPrinter("%.4f")

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42
  val paf = (x: Int) => x match {
    case 1 => 2
    case 2 => 4
  }

  println(byName(method))
  println(byName(parenMethod()))
  println(byName(parenMethod))
  println(byName(1))

  println(byFunction(() => method))
  println(byFunction(() => parenMethod()))
  println(byFunction(() => 1))
  println(byFunction(method _))
}
