package org.scala.learn
package collection

object Collections extends App {

  val numbers = List(1, 2, 3)
  val chars = List('a', 'b', 'c')
  val days = List("Sun", "Mon")

  println(numbers.flatMap(number => chars.flatMap(char => days.map(day => s"$char$number$day"))))

  val combination = for {
    number <- numbers // if number % 2 == 0
    day <- days
    char <- chars
  } yield s"$char$number$day"
  println(combination)

  println(2 :: 3 :: Nil)
  println(2 :: 3 :: Nil)
  println(1 +: (2 :: 3 :: Nil))
  println((2 :: 3 :: Nil) :+ 4)
  println(List.fill(10)(4))
  println(List.fill(10)(4).mkString(" | "))

  def printArr[T](array: Array[T]): Unit = array.foreach(println)

  val array = Array(1, 2, 3, 4)
  printArr(array)
  val arr2 = Array.ofDim[Int](3)
  printArr(arr2)
  array(2) = 5
  array.update(1, 2)
  printArr(array)
}
