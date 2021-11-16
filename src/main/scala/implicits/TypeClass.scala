package org.scala.learn
package implicits

object TypeClass extends App {

  trait Equals[T] {
    def ===(first: T, second: T): Boolean
  }

  implicit class EqualityEnricher[T](any: T) {
    def ===(other: T)(implicit equals: Equals[T]): Boolean = equals.===(any, other)
    def !==(other: T)(implicit equals: Equals[T]): Boolean = !equals.===(any, other)
  }

  object Equals {
    def apply[T](first: T, second: T)(implicit equals: Equals[T]): Boolean = equals.===(first, second)
  }

  case class Person(name: String)

  case class Building(levelsCount: Int)

  implicit object PersonEquals extends Equals[Person] {
    override def ===(first: Person, second: Person): Boolean = first.name == second.name
  }

  implicit object BuildingEquals extends Equals[Building] {
    override def ===(first: Building, second: Building): Boolean = first.levelsCount == second.levelsCount
  }

  object EqualityCheck {
    def check[T](first: T, second: T)(implicit equals: Equals[T]): Boolean =
      equals.===(first, second)

    def apply[T](implicit equals: Equals[T]): Equals[T] = equals
  }

  println(EqualityCheck[Person].===(Person("Ahmed"), Person("Ahmed")))
  println(EqualityCheck[Person].===(Person("Ahmed"), Person("Khalifa")))
  println(EqualityCheck[Building].===(Building(12), Building(24)))
  println(EqualityCheck[Building].===(Building(12), Building(12)))

  println()
  println(Equals(Person("Ahmed"), Person("Ahmed")))
  println(Equals(Building(12), Building(12)))

  println(Person("Ahmed") === Person("Ahmed1"))
  println(Person("Ahmed2") !== Person("Ahmed1"))
  println(Building(12) === Building(12))
}
