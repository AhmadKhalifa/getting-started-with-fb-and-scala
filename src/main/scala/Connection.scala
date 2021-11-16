package org.scala.learn

import scala.util.Random

class Connection() {
  override def toString: String = "Connected"
}

object Connection {

  val random = new Random()

  def apply(ip: String, port: String): Option[Connection] =
    if (random.nextBoolean()) Some(new Connection())
    else None
}

object TestConnection extends App {
  val configs = Map("ip" -> "192.168.1.1", "port" -> "8080")
  val connection = for {
    ip <- configs.get("ip")
    port <- configs.get("port")
    connection <- Connection(ip, port)
  } yield connection
  println(connection)
}