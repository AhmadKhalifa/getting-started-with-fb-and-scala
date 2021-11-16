package org.scala.learn
package collection.set

case class PropertyBasedSet[T](predicate: T => Boolean) extends Set[T] {
  override def isEmpty: Boolean = false

  override def head: T = throw new NoSuchElementException

  override def tail: Set[T] = throw new NoSuchElementException

  override def buildString: String = "Any element matches predicate"

  override def +(element: T): Set[T] =
    PropertyBasedSet(value => predicate(value) || value == element)

  override def -(element: T): Set[T] =
    filter(_ != element)
//    PropertyBasedSet(value => predicate(value) && element != value)

  override def ++(otherSet: Set[T]): Set[T] =
    PropertyBasedSet(value => predicate(value) || otherSet(value))

  override def &(otherSet: Set[T]): Set[T] = filter(otherSet)
//    PropertyBasedSet(value => predicate(value) && otherSet(value))

  override def --(otherSet: Set[T]): Set[T] = filter(!otherSet)
//    PropertyBasedSet(value => predicate(value) && !otherSet(value))

  override def contains(element: T): Boolean = predicate(element)

  override def filter(predicate: T => Boolean): Set[T] =
    PropertyBasedSet(value => predicate(value) && this.predicate(value))

  override def map[S](transformer: T => S): Set[S] =
    throw new RuntimeException("Can't map a property-based set")

  override def flatMap[S](transformer: T => Set[S]): Set[S] =
    throw new RuntimeException("Can't flat-map a property-based set")

  override def foreach(consumer: T => Unit): Unit =
    throw new RuntimeException("Can't iterate a property-based set")

  override def unary_! : Set[T] =
    PropertyBasedSet(value => !predicate(value))
}
