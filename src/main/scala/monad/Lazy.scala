package org.scala.learn
package monad

class Lazy[+T](valueByName: => T) {

  lazy val value: T = valueByName

  def apply(): T = value

  def flatMap[S](mapper: (=> T) => Lazy[S]): Lazy[S] = mapper(value)

  def map[S](mapper: (=> T) => Lazy[Lazy[S]]): Lazy[S] = flatMap(x => mapper(x).value)

  def flatten[S](nested: Lazy[Lazy[S]]): Lazy[S] = nested.flatMap(value => value)
}

object Lazy {

  def apply[T](value: => T): Lazy[T] = new Lazy(value)
}

object LazyTest extends App {
  val `lazy` = Lazy {
    println("Hello")
    "World"
  }

  private val value: Lazy[String] = `lazy`.flatMap(message => Lazy { s"Hello $message" })
  private val value1: Lazy[String] = `lazy`.flatMap(message => Lazy { s"Hello $message" })
  println("Not yet")
  println(value.value)
  println(value1.value)
}
