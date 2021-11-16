package org.scala.learn
package collection.stream

case object Empty extends Stream[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: Stream[Nothing] = throw new NoSuchElementException

  override def ++[S >: Nothing](otherStream: => Stream[S]): Stream[S] = otherStream

  override def filter(predicate: Nothing => Boolean): Stream[Nothing] = Empty

  override def map[S](transformer: Nothing => S): Stream[S] = Empty

  override def flatMap[S](transformer: Nothing => Stream[S]): Stream[S] = Empty

  override def foreach(consumer: Nothing => Unit): Unit = ()

  override def take(n: Int): Stream[Nothing] = Empty

  override def takeAsList(n: Int): List[Nothing] = Nil
}
