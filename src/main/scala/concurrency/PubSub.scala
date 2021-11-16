package org.scala.learn
package concurrency

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random


class Container[T](capacity: Int) {

  private val queue: mutable.Queue[T] = mutable.Queue()

  def isEmpty: Boolean = queue.isEmpty

  def publish(value: T): Unit = queue.enqueue(value)

  def read(): T = queue.dequeue()

  def isFull: Boolean = queue.size == capacity

  override def toString: String = s"Container($capacity)"
}

class Subscriber[T](container: Container[T], val name: String) extends Thread {

  override def run(): Unit = {
    while (true) {
      container.synchronized {
        while (container.isEmpty) {
          println(s"[$name] Waiting for a value to be published")
          container.wait()
        }
        println(s"[$name] Received a value from container $container: ${container.read()}\n\n")
        container.notify()
      }
      Thread.sleep(Random.nextInt(500))
    }
  }

  start()
}

class Publisher[T](container: Container[T], supplier: => () => T, messages: Int, name: String) extends Thread {

  @tailrec
  private def publish(messagesLeft: Int): Unit = {
    if (messagesLeft == 0) {
      println(s"[$name] No more messages to publish!")
      return
    }
    val value = supplier()
    println(s"[$name] Value calculated: $value")
    container.synchronized {
      if (container.isFull) {
        println(s"[$name] Awaiting for the container to have empty space...")
        container.wait()
      }
      println(s"[$name] Updating container with value $value\n")
      container.publish(value)
      container.notify()
    }
    Thread.sleep(Random.nextInt(500))
    publish(messagesLeft - 1)
  }

  override def run(): Unit = publish(messages)

  start()
}


object PubSubTest extends App {

  def calculateValue(consumerNumber: Int, delay: Long = 1): String = {
    Thread.sleep(delay)
    s"$consumerNumber=>${UUID.randomUUID().toString}"
  }

  def buildPublishersAndConsumers(container: Container[String], publishersCount: Int, subscribersCount: Int): Unit = {
    (1 to publishersCount)
      .foreach(id => new Publisher(container, () => calculateValue(id), Random.nextInt(10), s"Publisher-$id"))
    (1 to subscribersCount)
      .foreach(id => new Subscriber(container,s"Subscriber-$id"))
  }

  buildPublishersAndConsumers(new Container[String](3), 4, 2)
}
