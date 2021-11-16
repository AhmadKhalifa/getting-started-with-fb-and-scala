package org.scala.learn

object MatchExpressions extends App {

  class User(val name: String, val age: Int)

  object User {
    def unapply(arg: User): Option[(String, Int)] = Option((user.name, user.age))

    def unapply(age: Int): Option[String] =
      if (age >= 21) Some("Grown")
      else None
  }

  val user = new User("Bob", 23)

  val message = user match {
    case User(_, age) => s"I'm $age years old"
  }

  println(message)

  val age = 20

  def getAgeDescription(age: Int): String =
    age match {
      case User(ageDescription) => s"I'm $ageDescription"
      case _ => "I don't know"
    }
  println(getAgeDescription(12))
  println(getAgeDescription(21))

//  object even {
//    def unapply(number: Int): Option[Boolean] =
//      Some(number % 2 == 0)
//  }
//
//  object oneDigit {
//    def unapply(number: Int): Option[Boolean] =
//      Option(number)
//        .map(_.toString)
//        .map(_.length)
//        .map(_ == 1)
//  }
//
//  def checkNumberProperties(number: Int): String =
//    number match {
//      case oneDigit(matches) if matches => "One digit"
//      case even(matches) => if (matches) "Even" else "Odd"
//    }

  object even {
    def unapply(number: Int): Boolean = number % 2 == 0
  }

  object odd {
    def unapply(number: Int): Boolean = number % 2 == 1
  }

  object oneDigit {
    def unapply(number: Int): Boolean = number.toString.length == 1
  }

  def checkNumberProperties(number: Int): String =
    number match {
      case oneDigit() => "One digit"
      case even() => "Even"
      case odd() => "Odd"
    }

  println(checkNumberProperties(1))
  println(checkNumberProperties(2))
  println(checkNumberProperties(23))
  println(checkNumberProperties(22))

  sealed abstract class AbstractList[+T] {
    def head: T
    def tail: AbstractList[T]
  }

  case class List[+T](override val head: T, override val tail: AbstractList[T] = EmptyList) extends AbstractList[T]
  case object EmptyList extends AbstractList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException
    override def tail: AbstractList[Nothing] = throw new NoSuchElementException
  }

  object AbstractList {
    def unapply[T](list: AbstractList[T]): Option[Seq[T]] =
      if (list == EmptyList) Some(Seq.empty)
      else unapply(list.tail).map(list.head +: _)
  }

  object AbstractList1 {
    def unapplySeq[T](list: AbstractList[T]): Option[Seq[T]] =
      if (list == EmptyList) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val customList = List(5, List(2, List(3, List(15))))
  val list = customList match {
    case AbstractList1(1, 2, 3, 4) => "From one to four"
    case AbstractList1(1, _*) => "Starting with one"
    case seq @ AbstractList1(9, _*) => s"Starting with nine $seq"
    case AbstractList(10 :: Seq(_*)) | AbstractList(20 +: Seq(_*)) => "Starting with 10 or 20"
    case AbstractList(5 +: Seq(_*) :+ 15) => "Starting with 5 and ending with 15"
    case startingWithFive @ AbstractList(5 :: Seq(_*)) => s"Starting with five $startingWithFive"
    case AbstractList(3 +: (listPostfix @ Seq(_*))) => s"Starting with three with postfix $listPostfix"
    case AbstractList((listPrefix @ Seq(_*)) :+ 6) => s"Ending with six with prefix $listPrefix"
    case AbstractList(Seq(1, 2, _*)) => "Starting with 1, 2, ..."
    case otherList @ _ => s"Something else $otherList"
  }
  println(list)
}
