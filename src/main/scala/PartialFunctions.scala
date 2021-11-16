package org.scala.learn

import scala.util.Try

object PartialFunctions extends App {
  val partialFunction: PartialFunction[Int, String] = {
    case 0 => "ZERO"
    case 1 => "One"
  }
  println(Try { partialFunction(0) })
  println(Try { partialFunction(1) })
  println(Try { partialFunction(2) })
  println(partialFunction.isDefinedAt(1))
  println(partialFunction.isDefinedAt(2))
  println(partialFunction.lift(0))
  println(partialFunction.lift(1))
  println(partialFunction.lift(2))

  val manualPartialFunction = new PartialFunction[String, String] {

    override def apply(x: String): String = x match {
      case "0" => "Zero"
      case "1" => "One"
      case "2" => "Two"
    }

    override def isDefinedAt(v1: String): Boolean = List("0", "1").contains(v1)
  }

  println(manualPartialFunction("2"))
  scala.io.Source.stdin.getLines.map(manualPartialFunction).foreach(println)
}
