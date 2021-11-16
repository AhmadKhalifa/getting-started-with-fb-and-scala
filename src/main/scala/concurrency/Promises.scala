package org.scala.learn
package concurrency

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Failure, Random, Success, Try}

object Promises extends App {

  def immediateFutureValue(): Unit = {
    val promise = Promise[Int]
    val future = promise.future
    promise.success(4)
    println(Await.result(future, 3 seconds))
  }
  //  immediateFutureValue()

  def sequencedFutures(): Unit = {
    def sequence[T <: () => U, S <: () => U, U](firstFuture: Future[T], secondsFuture: Future[S]): Future[U] =
      firstFuture.map(_ ()).flatMap(_ => secondsFuture).map(_ ())

    val future1 = Future {
      () => {
        Thread.sleep(1000)
        val message = "Hello"
        println(message)
        message
      }
    }

    val future2 = Future {
      () => {
        Thread.sleep(200)
        val message = "World"
        println(message)
        message
      }
    }
    Await.ready(sequence[() => String, () => String, String](future1, future2), 2 seconds)
  }
  //  sequencedFutures()

  case class Lock(var isFirstFutureFinished: Boolean = false)

  def firstFuture(): Unit = {

    def firstFuture[T](firstFuture: Future[T], secondFuture: Future[T]): Future[T] = {
      val lock = Lock()

      val promise = Promise[T]
      val onCompete: Try[T] => Unit = {
        case Success(value) =>
          lock.synchronized {
            if (!lock.isFirstFutureFinished) {
              promise.success(value)
              lock.isFirstFutureFinished = true
            }
          }
        case Failure(exception) =>
          lock.synchronized {
            if (!lock.isFirstFutureFinished) {
              promise.failure(exception)
            }
          }
      }

      firstFuture.onComplete(onCompete)
      secondFuture.onComplete(onCompete)

      promise.future
    }

    def firstFutureV2[T](future1: Future[T], future2: Future[T]): Future[T] = {
      val promise = Promise[T]
      future1.onComplete(promise.tryComplete)
      future2.onComplete(promise.tryComplete)
      promise.future
    }

    val future1 = Future {
      Thread.sleep(Random.nextInt(1000))
      println("Hello finished")
      "Hello"
    }

    val future2 = Future {
      Thread.sleep(Random.nextInt(1000))
      println("World finished")
      "World"
    }

    println(Await.result(firstFuture(future1, future2), 2 seconds))
  }
  //  firstFuture()

  def lastFuture(): Unit = {

    def lastFuture[T](firstFuture: Future[T], secondFuture: Future[T]): Future[T] = {

      case class Lock(var isFirstFutureFinished: Boolean = false)
      val lock = Lock()

      val promise = Promise[T]
      val onCompete: Try[T] => Unit = {
        case Success(value) => lock.synchronized {
          if (lock.isFirstFutureFinished) promise.success(value)
          else lock.isFirstFutureFinished = true
        }
        case Failure(exception) => lock.synchronized {
          if (lock.isFirstFutureFinished) promise.failure(exception)
          else lock.isFirstFutureFinished = true
        }
      }

      firstFuture.onComplete(onCompete)
      secondFuture.onComplete(onCompete)

      promise.future
    }

    def lastFutureV2[T](future1: Future[T], future2: Future[T]): Future[T] = {
      val promise = Promise[T]
      val bothPromise = Promise[T]
      val checkPromise: Try[T] => Unit = trial => if (!bothPromise.tryComplete(trial)) promise.complete(trial)
      future1.onComplete(checkPromise)
      future2.onComplete(checkPromise)
      promise.future
    }

    val future1 = Future {
      Thread.sleep(Random.nextInt(1000))
      println("Hello finished")
      "Hello"
    }

    val future2 = Future {
      Thread.sleep(Random.nextInt(1000))
      println("World finished")
      "World"
    }

    println(Await.result(lastFuture(future1, future2), 2 seconds))
  }
  //  lastFuture()

  def retryUntil(): Unit = {

    var counter = 0

    def retryUntil[T](futureSupplier: => Future[T], predicate: T => Boolean): Future[T] =
      futureSupplier
        .filter(predicate)
        .recoverWith {
          case _: Exception => retryUntil(futureSupplier, predicate)
        }


    def getFutureWithRetries[T](future: => Future[T], retriesLeft: Int): Future[T] = {
      if (retriesLeft == 0) future
      else {
        lazy val fallback = getFutureWithRetries(future, retriesLeft - 1)
        future.recoverWith { // fallbackTo is eager while recoverWith is lazy
          case _: Throwable => (() => fallback)()
        }
      }
    }


    def future() = Future {
      if (Random.nextInt(100) > 75){
        val x = Random.nextInt(1000)
        println(s"Future returning $x")
        x
      } else {
        println("Future failed")
        throw new RuntimeException("No")
      }
    }

    def future1: Future[Int] = Future {
      val value = Random.nextInt(100)
      println(s"======> $value")
      value
    }

//    retryUntil(future(), () => counter == 10)
//    Thread.sleep(10000)

    println(Await.result(retryUntil(future1, (value: Int) => value.toString.endsWith("0")), 10 seconds))
//    println(Await.result(getFutureWithRetries(future(), 5), 3 seconds))

  }

  retryUntil()
}















