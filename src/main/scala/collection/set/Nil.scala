package org.scala.learn
package collection.set

case class Nil[T]() extends Set[T] {

  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException

  override def tail: Set[T] = throw new NoSuchElementException

  override def buildString: String = ""

  override def +(element: T): Set[T] = ::(element, this)

  override def -(element: T): Set[T] = Nil()

  override def ++(otherSet: Set[T]): Set[T] = otherSet

  override def &(otherSet: Set[T]): Set[T] = Nil()

  override def --(otherSet: Set[T]): Set[T] = Nil()

  override def contains(element: T): Boolean = false

  override def filter(predicate: T => Boolean): Set[T] = Nil()

  override def map[S](transformer: T => S): Set[S] = Nil()

  override def flatMap[S](transformer: T => Set[S]): Set[S] = Nil()

  override def foreach(consumer: T => Unit): Unit = ()

  override def unary_! : Set[T] = new PropertyBasedSet[T](_ => true)
}
