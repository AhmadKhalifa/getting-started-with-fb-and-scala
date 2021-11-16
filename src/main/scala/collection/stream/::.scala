package org.scala.learn
package collection.stream

class ::[+T](override val head: T, tailStream: => Stream[T]) extends Stream[T] {

  override def isEmpty: Boolean = false

  override lazy val tail: Stream[T] = tailStream

  override def ++[S >: T](otherStream: => Stream[S]): Stream[S] = new ::(head, tail ++ otherStream)

  override def filter(predicate: T => Boolean): Stream[T] =
    if (predicate(head)) new ::(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def map[S](transformer: T => S): Stream[S] =
    new ::(transformer(head), tail.map(transformer))

  override def flatMap[S](transformer: T => Stream[S]): Stream[S] =
    transformer(head) ++ tail.flatMap(transformer)

  override def foreach(consumer: T => Unit): Unit = {
    consumer(head)
    tail.foreach(consumer)
  }

  override def take(n: Int): Stream[T] = n match {
    case x if x <= 0 => Empty
    case 1 => new ::(head, Empty)
    case _ => head #:: tail.take(n - 1)
  }

  override def takeAsList(n: Int): List[T] = (this take n).toList
}
