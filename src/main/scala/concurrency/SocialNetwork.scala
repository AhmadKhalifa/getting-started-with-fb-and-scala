package org.scala.learn
package concurrency

import concurrency.SocialNetwork._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

object SocialNetwork {

  case class Profile(name: String)

  val profiles = Map(
    1 -> Profile("Khalifa"),
    2 -> Profile("Omar"),
    3 -> Profile("Hassan")
  )

  val friends = Map(
    1 -> 2,
    2 -> 3,
    3 -> 1
  )

  def fetchProfile(userId: Int): Future[Profile] =
    Future {
      Thread.sleep(Random.nextInt(200))
      profiles(userId)
    }

  def fetchBestFriendProfile(userId: Int): Future[Profile] =
    Future {
      Thread.sleep(Random.nextInt(200))
      friends(userId)
    }.flatMap(fetchProfile)

  def pokeBestFriend(userId: Int): Unit = {
    fetchProfile(userId)
      .onComplete {
        case Success(profile) => {
          fetchBestFriendProfile(userId)
            .onComplete {
              case Success(bestFriendProfile) => {
                println(s"Hmmmm. ${profile.name} has poked his best friend ${bestFriendProfile.name}")
              }
              case Failure(exception) => exception.printStackTrace()
            }
        }
        case Failure(exception) => exception.printStackTrace()
      }
  }

  def pokeBestFriendWithBetterApproach(userId: Int): Unit =
    Future(userId)
      .flatMap(fetchProfile)
      .flatMap(profile => fetchBestFriendProfile(userId).map(friendProfile => (profile, friendProfile)))
      .map((profiles: (Profile, Profile)) => s"${profiles._1.name} has poked his best friend ${profiles._2.name}")
      .onComplete {
        case Success(value) => println(value)
        case Failure(exception) => println(exception)
      }

  def pokeBestFriendWithForComprehension(userId: Int): Unit =
    (for {
      profile <- fetchProfile(userId)
      bestFriendProfile <- fetchBestFriendProfile(userId)
    } yield s"${profile.name} has super-poked his best friend ${bestFriendProfile.name}")
      .onComplete {
        case Success(value) => println(value)
        case Failure(exception) => println(exception)
      }
}


object SocialMediaTest extends App {

  private val userId = 1

  pokeBestFriend(userId)

  pokeBestFriendWithBetterApproach(userId)

  pokeBestFriendWithForComprehension(userId)

  val ghostFuture: Future[Profile] = fetchProfile(5)
    .recover {
      case _: Throwable => Profile("Ghost")
    }

  val anotherGhostFuture: Future[Profile] = fetchProfile(6)
    .recoverWith {
      case _: Throwable => fetchProfile(userId)
    }

  val anotherAnotherGhostFuture: Future[Profile] = fetchProfile(7)
    .fallbackTo(fetchProfile(1))

  println("======> " + Await.ready(ghostFuture, 2.seconds))
  println("===> " + Await.result(anotherGhostFuture, 2.seconds))
  Await.ready(anotherAnotherGhostFuture, 2.seconds)
}
