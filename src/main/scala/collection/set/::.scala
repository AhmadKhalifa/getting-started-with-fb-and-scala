package org.scala.learn
package collection.set

case class ::[T](override val head: T, override val tail: Set[T] = Nil()) extends Set[T] {

  override def isEmpty: Boolean = false

  override def buildString: String = if (tail.isEmpty) head.toString else s"$head, ${tail.buildString}"

  override def +(element: T): Set[T] =
    if (contains(element)) this
    else ::(element, this)

  override def -(element: T): Set[T] =
    if (head == element) tail
    else head +: (tail - element)

  override def ++(otherSet: Set[T]): Set[T] =
    tail ++ otherSet + head
//    if (set.contains(head)) tail ++ set
//    else ::(head, tail ++ set)

  override def &(otherSet: Set[T]): Set[T] = filter(otherSet)

  override def --(otherSet: Set[T]): Set[T] = filter(!otherSet)

  override def contains(element: T): Boolean =
    if (head == element) true
    else if (tail.isEmpty) false
    else tail contains element

  override def filter(predicate: T => Boolean): Set[T] =
    if (predicate(head)) ::[T](head, tail filter predicate)
    else tail filter predicate

  override def map[S](transformer: T => S): Set[S] =
    ::[S](transformer(head), tail map transformer)

  override def flatMap[S](transformer: T => Set[S]): Set[S] =
    transformer(head) ++ (tail flatMap transformer)

  override def foreach(consumer: T => Unit): Unit = {
    consumer(head)
    tail foreach consumer
  }

  override def unary_! : Set[T] = PropertyBasedSet(!this(_))
}
