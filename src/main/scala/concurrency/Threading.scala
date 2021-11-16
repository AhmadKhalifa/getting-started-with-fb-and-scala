package org.scala.learn
package concurrency

import scala.annotation.tailrec
import scala.util.Random

object Threading extends App {

  def printMessage(threadNumber: Int): Thread = {
    val thread = new Thread(() => {
      if (threadNumber > 0) {
        val innerThread = printMessage(threadNumber - 1)
        innerThread.start()
        innerThread.join()
      }
      println(s"Hello from thread ${Thread.currentThread.getName}")
    })
    thread.setName(s"Thread-$threadNumber")
    thread
  }

  //  private val thread: Thread = printMessage(5)
  //  thread.start()
  //  thread.join()

  @tailrec
  def runMultipleTimes(times: Int, min: Int = Int.MaxValue, max: Int = Int.MinValue): (Int, Int) = {
    if (times == 0) (min, max)
    else {
      println(s"Calculating x for the $times time")
      var x = 0
      val threads = (1 to 100).map(_ => new Thread(() => {
        Thread.sleep(100)
        x += 1
      }))
      threads.foreach(_.start)
      threads.foreach(_.join)
      runMultipleTimes(times - 1, Math.min(min, x), Math.max(max, x))
    }
  }

  //  val (min, max) = runMultipleTimes(100)
  //  println(s"Min: $min\nMax: $max")

  def demoSleepFallacy(): Unit = {
    var message = ""
    val thread = new Thread(() => {
      Thread.sleep(1000)
      message = "Let's go Cena"
    })
    message = "Cena sucks"
    thread.start()
    Thread.sleep(1001)
    println(message)
  }

  //  demoSleepFallacy()

  class Container[T] {

    private var _value: Option[T] = None

    def isEmpty: Boolean = _value.isEmpty

    def value: T = {
      val result = _value.get
      _value = None
      result
    }

    def value_=(newValue: T): Unit = {
      _value = Some(newValue)
    }
  }

  def calculateValue(delay: Long = 1000): Int = {
    Thread.sleep(delay)
    new Random().nextInt()
  }

  def naiveProducerConsumer(): Unit = {

    class Consumer[T](val container: Container[T]) extends Thread {

      override def run(): Unit = {
        println(s"[Consumer@${Thread.currentThread.getName}] Listening on container $container")
        while (container.isEmpty) {
          if (System.currentTimeMillis() % 5000 == 0) {
            println(s"[Consumer@${Thread.currentThread.getName}] Still listening on container $container")
          }
        }
        println(s"[Consumer@${Thread.currentThread.getName}] Received a value from container $container: ${container.value}")
      }
    }

    class Producer[T](val container: Container[T], supplier: () => T) extends Thread {

      override def run(): Unit = {
        println(s"[Producer@${Thread.currentThread.getName}] Calculating value")
        val value = supplier()
        println(s"[Producer@${Thread.currentThread.getName}] Value calculated: $value, updating container $container")
        container.value = value
      }
    }

    def calculateValue(): Int = {
      Thread.sleep(1000)
      new Random().nextInt()
    }

    val container = new Container[Int]
    val consumer = new Consumer(container)
    consumer.start()
    val producer = new Producer(container, calculateValue)
    producer.start()
    producer.join()
    consumer.join()
  }


  def waitNotifyProducerConsumer(): Unit = {
    class Consumer[T](val container: Container[T]) extends Thread {

      override def run(): Unit = {
        container.synchronized {
          println(s"[Consumer@${Thread.currentThread.getName}] Listening on container $container")
          container.wait()
          println(s"[Consumer@${Thread.currentThread.getName}] Received a value from container $container: ${container.value}")
        }
      }
    }

    class Producer[T](val container: Container[T], supplier: () => T) extends Thread {

      override def run(): Unit = {
        println(s"[Producer@${Thread.currentThread.getName}] Calculating value")
        val value = supplier()
        println(s"[Producer@${Thread.currentThread.getName}] Value calculated: $value, updating container $container")
        container.synchronized {
          container.value = value
          container.notify()
        }
      }
    }

    val container = new Container[Int]
    val consumer = new Consumer(container)
    consumer.start()
    val producer = new Producer(container, () => calculateValue(3000))
    producer.start()
    producer.join()
    consumer.join()
  }

//  waitNotifyProducerConsumer()

  case class Friend(name: String) {

    def bow(other: Friend): Unit = {
      this.synchronized {
        println(s"I'm bowing to my friend $other")
        other.rise(this)
        println(s"My friend $other has risen to me")
      }
    }

    def rise(other: Friend): Unit = {
      this.synchronized {
        println(s"I'm rising to my friend $other")
      }
    }

    var side: String = "right"
    def otherSide(): String = if (side == "right") "left" else "right"

    def pass(other: Friend): Unit = {
      while (side == other.side) {
        println(s"Oh my friend $other please pass")
        side = otherSide()
        Thread.sleep(1000)
      }
    }
  }


  val omar = Friend("Omar")
  val khalifa = Friend("Khalifa")

  def simulateDeadlock(): Unit = {
    new Thread(() => omar.bow(khalifa)).start()
    new Thread(() => khalifa.bow(omar)).start()
  }

//  simulateDeadlock()

  def simulateLivelock(): Unit = {
    new Thread(() => omar.pass(khalifa)).start()
    new Thread(() => khalifa.pass(omar)).start()
  }

//  simulateLivelock()
}
