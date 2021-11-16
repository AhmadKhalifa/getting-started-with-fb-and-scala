package org.scala.learn
package collection.list

case object Nil extends List[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: List[Nothing] = throw new NoSuchElementException

  override def buildString: String = ""

  override def add[T >: Nothing](element: T): List[T] = ::(element, Nil)

  override def ++[S >: Nothing](list: List[S]): List[S] = list

  override def concatTailRec[S >: Nothing](list: List[S]): List[S] = list

  override def filter(predicate: Nothing => Boolean): List[Nothing] = Nil

  override def filterTailRec(predicate: Nothing => Boolean): List[Nothing] = Nil

  override def map[S](transformer: Nothing => S): List[S] = Nil

  override def mapTailRec[S](transformer: Nothing => S): List[S] = Nil

  override def flatMap[S](transformer: Nothing => List[S]): List[S] = Nil

  override def flatMapTailRec[S](transformer: Nothing => List[S]): List[S] = Nil

  override def foreach(consumer: Nothing => Unit): Unit = ()

  override def sort(comparator: (Nothing, Nothing) => Int): List[Nothing] = Nil

  override def zipWith[S, U](otherList: List[S], merger: (Nothing, S) => U): List[U] =
    if (!otherList.isEmpty) throw new IllegalArgumentException("Lists don't have the same length")
    else Nil

  override def fold[S >: Nothing](initialValue: S)(folder: (S, S) => S): S = initialValue
}
