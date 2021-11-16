package org.scala.learn
package concurrency

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object Futures extends App {

  def calculateValue: Int = {
    Thread.sleep(Random.nextInt(3000))
    Random.nextInt(1000)
  }

  val future = Future { calculateValue }

  future.onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(s"Error calculating value: $exception")
  }



}
