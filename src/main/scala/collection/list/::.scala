package org.scala.learn
package collection.list

import scala.annotation.tailrec

case class ::[+T](override val head: T, override val tail: List[T] = Nil) extends List[T] {

  override def isEmpty: Boolean = false

  override def buildString: String = if (tail.isEmpty) head.toString else s"$head, ${tail.buildString}"

  override def add[S >: T](element: S): List[S] = ::(element, this)

  override def ++[S >: T](list: List[S]): List[S] = ::[S](head, tail ++ list)

  override def concatTailRec[S >: T](list: List[S]): List[S] = {

    @tailrec
    def addHead(toBeAddedList: List[S], accumulatedList: List[S]): List[S] =
      if (toBeAddedList.isEmpty) accumulatedList
      else addHead(toBeAddedList.tail, accumulatedList.add(toBeAddedList.head))

    addHead(list, this)
  }

  override def filter(predicate: T => Boolean): List[T] =
    if (predicate(head)) ::[T](head, tail.filter(predicate))
    else tail.filter(predicate)

  override def filterTailRec(predicate: T => Boolean): List[T] = {

    @tailrec
    def filter(list: List[T], filteredList: List[T]): List[T] =
      if (list.isEmpty) filteredList
      else filter(list.tail, if (predicate(list.head)) filteredList.add(list.head) else filteredList)

    filter(this, Nil)
  }

  override def map[S](transformer: T => S): List[S] =
    ::[S](transformer(head), tail.map(transformer))

  override def mapTailRec[S](transformer: T => S): List[S] = {

    @tailrec
    def map(list: List[T], mappedList: List[S]): List[S] =
      if (list.isEmpty) mappedList
      else map(list.tail, mappedList.add(transformer(list.head)))

    map(this, Nil)
  }

  override def flatMap[S](transformer: T => List[S]): List[S] =
    transformer(head) ++ tail.flatMap(transformer)

  override def flatMapTailRec[S](transformer: T => List[S]): List[S] = {

    @tailrec
    def map(list: List[T], mappedList: List[S]): List[S] =
      if (list.isEmpty) mappedList
      else map(list.tail, mappedList ++ transformer(list.head))

    map(this, Nil)
  }

  override def foreach(consumer: T => Unit): Unit = {
    consumer(head)
    tail.foreach(consumer)
  }

  override def sort(comparator: (T, T) => Int): List[T] = {

    def insert(element: T, list: List[T]): List[T] =
      if (list.isEmpty) ::(element)
      else if (comparator(element, list.head) <= 0) ::(head, list)
      else ::(list.head, insert(element, list.tail))

    insert(head, tail.sort(comparator))
  }

  override def zipWith[S, U](otherList: List[S], merger: (T, S) => U): List[U] =
    if (otherList.isEmpty) throw new IllegalArgumentException("Lists don't have the same length")
    else ::(merger(head, otherList.head)) ++ tail.zipWith(otherList.tail, merger)

  override def fold[S >: T](initialValue: S)(folder: (S, S) => S): S =
    tail.fold(folder(initialValue, head))(folder)
  //    folder(head, tail.fold(initialValue)(folder))
}
