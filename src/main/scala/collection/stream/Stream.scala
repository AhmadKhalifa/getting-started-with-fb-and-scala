package org.scala.learn
package collection.stream

import scala.annotation.tailrec

abstract class Stream[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: Stream[T]

  def #::[S >: T](element: S): Stream[S] = new ::(element, this)

  def ++[S >: T](otherStream: => Stream[S]): Stream[S]

  def filter(predicate: T => Boolean): Stream[T]

  def map[S](transformer: T => S): Stream[S]

  def flatMap[S](transformer: T => Stream[S]): Stream[S]

  def foreach(consumer: T => Unit): Unit

  def take(n: Int): Stream[T]

  def takeAsList(n: Int): List[T]

  def toList: List[T] = {

    @tailrec
    def buildList(stream: Stream[T], accumulator: List[T]): List[T] =
      if (stream.isEmpty) accumulator
      else buildList(stream.tail, accumulator :+ stream.head)

    buildList(this, Nil)
  }
}

object Stream {

  def apply[T](values: T*): Stream[T] = {

    @tailrec
    def buildStream(values: Seq[T], accumulatorStream: Stream[T]): Stream[T] =
      if (values.isEmpty) accumulatorStream
      else buildStream(values.tail, values.head #:: accumulatorStream)

    buildStream(values, Empty)
  }

  def from[T](start: T)(generator: T => T): Stream[T] =
    new ::(start, from(generator(start))(generator))
}

object StreamTest extends App {
  val stream1 = Stream(1, 2, 3)
  val stream2 = Stream(4, 5, 6)
  stream1 foreach println
  println
  stream2 foreach println
  println
  8 #:: stream1 map (_ - 1) filter (_ % 2 == 1) flatMap (value => Stream(value, value + 1)) foreach println
  println
  stream1 ++ stream2 foreach println
  println
  val infiniteStream = Stream.from(1)(_ + 1)
  infiniteStream take 10 foreach println
  println
  infiniteStream takeAsList 5 foreach println
  println
  infiniteStream flatMap (value => Stream(value, value * 100)) take 5 foreach println
  println
  println((infiniteStream map (_ * 2) take 5).toList)
  println
  infiniteStream filter (_ % 2 == 0) take 5 foreach println
  println

  def primeNumbers(initSize: Int = 1000): Stream[Int] = {

    @tailrec
    def filterOut(divisors: Stream[Int], primeNumbers: Stream[Int]): Stream[Int] = {
      if (divisors.isEmpty) primeNumbers
      else filterOut(divisors.tail, primeNumbers.filter(number => number == divisors.head || number % divisors.head != 0))
    }

    val numbers = Stream.from(2)(_ + 1).take(initSize)
    filterOut(numbers, numbers)
  }

  def primes(): Stream[Int] = {

    def buildPrimes(stream: Stream[Int]): Stream[Int] =
      new ::(stream.head, buildPrimes(stream.tail.filter(_ % stream.head != 0)))

    buildPrimes(Stream.from(2)(_ + 1))
  }

  def fibonacci: Stream[BigInt] = {

    def buildFibonacci(first: BigInt, second: BigInt): Stream[BigInt] =
      new ::(first + second, buildFibonacci(second, first + second))

    Stream(BigInt(1), BigInt(1)) ++ buildFibonacci(1, 1)
  }


  println("fibonacci")
  fibonacci take 100 foreach println
  println("Prime")
  primeNumbers() take 100 foreach println
  println("Prime2")
  primes() take 2000 foreach println
}