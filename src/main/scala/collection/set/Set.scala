package org.scala.learn
package collection.set

import scala.annotation.tailrec

abstract class Set[T] extends (T => Boolean) {

  override def apply(v1: T): Boolean = contains(v1)

  def isEmpty: Boolean

  def head: T

  def tail: Set[T]

  def buildString: String

  def +: (element: T): Set[T] = this + element

  def +(element: T): Set[T]

  def -(element: T): Set[T]

  def ++(otherSet: Set[T]): Set[T]

  def &(otherSet: Set[T]): Set[T]

  def --(otherSet: Set[T]): Set[T]

  def contains(element: T): Boolean

  def filter(predicate: T => Boolean): Set[T]

  def map[S](transformer: T => S): Set[S]

  def flatMap[S](transformer: T => Set[S]): Set[S]

  def foreach(consumer: T => Unit): Unit

//  def unary_!(element: T): Boolean = !this(element)

  def unary_! : Set[T]

  override def toString: String = s"Set [$buildString]"
}

object Set {

//  def apply[T](values: T*): Set[T] =
//    if (values.isEmpty) Nil[T]()
//    else values.head +: apply(values.tail : _*)

  def apply[T](values: T*): Set[T] = {

    @tailrec
    def buildSet(values: Seq[T], accumulatorSet: Set[T]): Set[T] =
      if (values.isEmpty) accumulatorSet
      else buildSet(values.tail, accumulatorSet + values.head)

    buildSet(values, Nil())
  }
}

object SetTest extends App {


//  def sum(args: Int*) : Int =
//    if (args.isEmpty)
//      0 else
//      args.head + sum(args.tail)

  def sum(args: Int*) : Int =
    if (args.isEmpty)
      0 else
      args.head + sum(args.tail : _*)

  println(sum(1, 2, 3))

//  val set1: Set[Int] = ::(1, ::(2, ::(3, Nil())))
//  val set2: Set[Int] = ::(2, ::(3, ::(4, Nil())))
//  val set1: Set[Int] = 1 +: 2 +: 3 +: 4 +: Nil()
//  val set2: Set[Int] = 1 +: 3 +: 4 +: 4 +: Nil()
  val set1 = Set(1, 2, 3, 4)
  val set2 = Set(1, 3, 4, 5)
  println(set1)
  println(set2)
  println(set1 ++ set2)

  println()
  set1 + 10 ++ Set(7, 8, 6, 9) map (_ * 3) flatMap (num => Set(num, num / 2)) filter (_ % 2 == 0) foreach println

  println(Set(1, 2, 3, 4) - 3)

  println(Set(1, 2, 3, 4, 5) & Set(3, 5, 6, 7))
  println(Set(1, 2, 3, 4, 5) -- Set(3, 5, 6, 7))

  println((! new Nil[Int])(2))
  println((!Set[Int](2))(2))

}
