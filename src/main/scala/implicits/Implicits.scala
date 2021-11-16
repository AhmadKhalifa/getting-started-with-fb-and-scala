package org.scala.learn
package implicits

import java.util.Comparator

object Implicits extends App {

  case class Person(name: String, age: Int)

  implicit val personOrdering: Ordering[Person] = // Ordering[(String, Int)] on ((p: Person) => (p.name, p.age))
    (Ordering[String] on[Person](_.name)).orElse((Ordering[Int] on[Person](_.age)).reverse)

  //  (firstPerson: Person, secondPerson: Person) =>
//    Comparator
//      .comparing((person: Person) => person.name)
//      .thenComparing(Comparator.comparingInt((person: Person) => person.age).reversed())
//      .compare(firstPerson, secondPerson)


  List(
    Person("Khalifa", 20),
    Person("Khalifa", 23),
    Person("Khalifa", 21),
    Person("Ahmed", 26),
    Person("Omar", 23)
  ).sorted foreach println
}
