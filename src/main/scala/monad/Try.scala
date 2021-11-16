package org.scala.learn
package monad

sealed trait Try[+T] {

  def map[S](transformer: T => S): Try[S]

  def flatMap[S](transformer: T => Try[S]): Try[S]

  def flatten[S >: T](nested: Try[Try[S]]): Try[S]
}

object Try {
  def apply[T](value: => T): Try[T] =
    try {
      Success(value)
    } catch {
      case throwable: Throwable => Failure(throwable)
    }
}

case class Success[+T](value: T) extends Try[T] {

  override def map[S](transformer: T => S): Try[S] = Try {
    transformer(value)
  }

  override def flatMap[S](transformer: T => Try[S]): Try[S] = transformer(value)

  override def flatten[S >: T](nested: Try[Try[S]]): Try[S] =
    nested match {
      case Success(value) => value
      case Failure(throwable) => throw throwable
    }
}

case class Failure(throwable: Throwable) extends Try[Nothing] {

  override def map[S](transformer: Nothing => S): Try[S] = this

  override def flatMap[S](transformer: Nothing => Try[S]): Try[S] = this

  override def flatten[S >: Nothing](nested: Try[Try[S]]): Try[S] = this
}

object TestTry extends App {
  println(
    Try {
      1
    }
  )

  println(
    Try {
      throw new RuntimeException("No")
    }
  )

  println(
    Try[Int] {
      throw new RuntimeException("No")
    }
      .map(_.toString)
  )

  println(
    Try {
      1
    }
      .map(_.toString)
  )

  println(
    Try {
      1
    }
      .flatMap(value =>
        Try {
          value.toString
        }
      )
  )

  println(
    Success(1)
      .flatten(Try {
        2
      }
        .map(value =>
          Try {
            value.toString
          }
        ))
  )
}
