package org.scala.learn
package collection.list

abstract class List[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def buildString: String

  def add[S >: T](element: S): List[S]

  def ++[S >: T](list: List[S]): List[S]

  def concatTailRec[S >: T](list: List[S]): List[S]

  def filter(predicate: T => Boolean): List[T]

  def filterTailRec(predicate: T => Boolean): List[T]

  def map[S](transformer: T => S): List[S]

  def mapTailRec[S](transformer: T => S): List[S]

  def flatMap[S](transformer: T => List[S]): List[S]

  def flatMapTailRec[S](transformer: T => List[S]): List[S]

  def foreach(consumer: T => Unit): Unit

  def sort(comparator: (T, T) => Int): List[T]

  def zipWith[S, U](otherList: List[S], merger: (T, S) => U): List[U]

  def fold[S >: T](initialValue: S)(folder: (S, S) => S): S

  override def toString: String = s"[$buildString]"
}

object ListTest extends App {
  val emptyIntList = Nil
  println(emptyIntList)
  println(emptyIntList.add(1))
  println(emptyIntList.add(10).add(20))
  println(emptyIntList.add(100).add(200).add(300))

  val emptyStringList = Nil
  println(emptyStringList)
  println(emptyStringList.add("1"))
  println(emptyStringList.add("10").add("20"))
  println(emptyStringList.add("100").add("200").add("300"))

  private val transformer: String => ::[Char] =
    (str: String) => new ::[Char](str.charAt(2), new ::[Char](str.charAt(0), Nil))

  println(emptyStringList.add("100").add("210").add("300").filter(_.endsWith("00")))
  println(emptyStringList.add("100").add("210").add("300").map(_.toInt * 2))
  println(emptyStringList.add("104").add("205").add("306").flatMap(transformer))

  println(emptyStringList.add("100").add("210").add("300").filterTailRec(_.endsWith("00")))
  println(emptyStringList.add("100").add("210").add("300").mapTailRec(_.toInt * 2))
  println(emptyStringList.add("104").add("205").add("306").flatMapTailRec(transformer))

  emptyIntList.add(1).add(2).add(3).foreach(printf("__%d__\n", _))
  println(emptyIntList.add(1).add(2).add(3).fold(10)(_ + _))
  println(emptyIntList.add(1).add(2).add(3).fold(1)(_ * _))

  println(emptyIntList.add(1).add(2).add(3).zipWith[Int, Int](emptyIntList.add(4).add(5).add(6), _ + _))

  println(emptyIntList.add(200).add(300).add(100).sort(_.compareTo(_)))

  val myList = emptyIntList.add(4).add(3).add(2).add(1)
  val numbers = emptyIntList.add(400).add(300).add(200).add(100)

  val mappedList1 = for { // map
    element <- myList
  } yield s".$element"
  println(mappedList1)

  val mappedList2 = for { // flatMap
    element <- myList
    number <- numbers
  } yield s".$element $number"
  println(mappedList2)

  for { // foreach
    element <- myList
  } println(element)
}
