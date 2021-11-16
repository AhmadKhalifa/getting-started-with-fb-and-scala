package org.scala.learn
package types

object HigherKindedTypes extends App {

  trait Monad[T[_], S] {

    def flatMap[U](transformer: S => T[U]): T[U]

    def map[U](transformer: S => U): T[U]

    def *[U](other: Monad[T, U]): T[(S, U)] =
      for {
        first <- this
        second <- other
      } yield (first, second)
  }

  implicit class ListMonad[T](val list: List[T]) extends Monad[List, T] {

    override def flatMap[U](transformer: T => List[U]): List[U] = list.flatMap(transformer)

    override def map[U](transformer: T => U): List[U] = list.map(transformer)
  }

  implicit class OptionMonad[T](val option: Option[T]) extends Monad[Option, T] {

    override def flatMap[U](transformer: T => Option[U]): Option[U] = option.flatMap(transformer)

    override def map[U](transformer: T => U): Option[U] = option.map(transformer)
  }

  println(Some(4) * Some("Hi"))
  println(List(1, 2, 3) * List("A", "B", "C"))
}
