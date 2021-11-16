package org.scala.learn

import scala.language.postfixOps

class Person(name: String, age: Int, favoriteMovie: String) {
  def +(nickName: String): Person = new Person(s"$name ($nickName)", age, favoriteMovie)

  def unary_+ : Person = new Person(name, this.age + 1, favoriteMovie)

  def learns(topic: String): Unit = println(s"$name learns $topic")

  //noinspection UnitMethodIsParameterless
  def learnsScala: Unit = this learns "Scala"

  def apply(times: Int): Unit = println(s"$name watched $favoriteMovie $times times")

  def getIntroduction: String = s"Hello I'm $name, $age years old, and I like to watch $favoriteMovie"

  override def toString: String = getIntroduction

  def apply(): Unit = println(this)
}


object TestPerson extends App {
  val khalifa = new Person("Khalifa", 26, "The dark knight")
  khalifa()
  println(+khalifa)
  println(khalifa + "Hazelnut")
  khalifa(2)
  khalifa learns "Functional programming"
  khalifa learnsScala
}
