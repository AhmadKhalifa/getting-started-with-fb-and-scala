package org.scala.learn
package collection.option

abstract sealed class Option[+T] protected() {

  def value: T

  val isEmpty: Boolean

  def filter(predicate: T => Boolean): Option[T]

  def map[S](mapper: T => S): Option[S]

  def flatMap[S](mapper: T => Option[S]): Option[S]
}

object Option {

  def apply[T](value: T): Option[T] =
    if (value == null) None
    else Some(value)
}

case class Some[+T](override val value: T) extends Option[T] {

  override val isEmpty: Boolean = false

  override def filter(predicate: T => Boolean): Option[T] =
    if (predicate(value)) Some(value)
    else None

  override def map[S](mapper: T => S): Option[S] = Option(mapper(value))

  override def flatMap[S](mapper: T => Option[S]): Option[S] = mapper(value)

  override def toString: String = s"Some($value)"
}

case object None extends Option[Nothing] {

  override def value: Nothing = throw new NoSuchElementException

  override val isEmpty: Boolean = true

  override def filter(predicate: Nothing => Boolean): Option[Nothing] = None

  override def map[S](mapper: Nothing => S): Option[S] = None

  override def flatMap[S](mapper: Nothing => Option[S]): Option[S] = None

  override def toString: String = "None"
}

object OptionTest extends App {
  val option = Option("Hi")
  println(option)
  println(option.map(_.length))
  println(option.filter(_.length > 100))
}
