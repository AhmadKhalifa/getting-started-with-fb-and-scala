package org.scala.learn

import TailRecursion.{concatString, fibonacci, isPrime}

import scala.annotation.tailrec

object TailRecursion {

  @tailrec
  def concatString(str: String, times: Int, accumulator: StringBuilder = new StringBuilder("")): String =
    if (times <= 0) accumulator.toString()
    else concatString(str, times - 1, accumulator.append(str))

  def isPrime(number: Int): Boolean = {

    @tailrec
    def isPrime(divisor: Int = 2): Boolean = {
      if (number <= 2) return number == 2
      if (number % divisor == 0) return false
      if (divisor * divisor > number) return true
      isPrime(divisor + 1)
    }

    isPrime()
  }

  def fibonacci(n: Int): Int = {

    @tailrec
    def fibonacci(currentIndex: Int, prevIndexResult: Int, prevPrevIndexResult: Int): Int = {
      if (n < currentIndex) return 1
      if (n == currentIndex) return prevIndexResult + prevPrevIndexResult
      fibonacci(currentIndex + 1, prevIndexResult + prevPrevIndexResult, prevIndexResult)
    }

    fibonacci(3, 1, 1)
  }
}

object TestTailRecursion extends App {
  println(concatString("Hello", 3))
  println(concatString("Hello", 1))
  println(concatString("Hello", -1))
  println(concatString("Hello", 0))

  println(isPrime(1))
  println(isPrime(2))
  println(isPrime(3))
  println(isPrime(4))
  println(isPrime(5))
  println(isPrime(6))
  println(isPrime(7))

  println(fibonacci(1))
  println(fibonacci(2))
  println(fibonacci(3))
  println(fibonacci(4))
  println(fibonacci(5))
  println(fibonacci(6))
}