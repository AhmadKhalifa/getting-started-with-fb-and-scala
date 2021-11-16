package org.scala.learn
package implicits

import scala.language.implicitConversions

object Pimping extends App {

  implicit class RichString(string: String) {
    def toInteger: Int = Integer.parseInt(string)
    def encrypt: String = string.map(char => (char + 2).asInstanceOf[Char])
  }

  implicit class RichInt(int: Int) {
    def isEven: Boolean = int % 2 == 0
    def sqrt: Double = Math.sqrt(int)
    def times(function: Int => Unit): Unit = (1 to int).foreach(function)
    def *[T](seq: Seq[T]): Seq[T] =
      if (int == 0) Nil
      else seq ++ ((int - 1) * seq)
  }

  implicit def stringToInt(string: String): Int = string.toInteger

  println("35".toInteger)
  println("Khalifa".encrypt)

  println(1.isEven)
  println(2.isEven)
  println(25.sqrt)
  println(412.sqrt)
  2 times println
  println(3 * List(1, 2))

  println("10" / 5)
}
