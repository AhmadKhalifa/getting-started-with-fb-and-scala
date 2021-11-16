package org.scala.learn
package collection.social

import scala.annotation.tailrec

case class SocialMedia(network: String Map Set[String] = Map()) {

  private def +(person: (String, Set[String])): SocialMedia =
    SocialMedia(network + person)

  private def -(person: String): SocialMedia =
    SocialMedia(network.filter(_._1 != person))

  def +(person: String): SocialMedia =
    this + (person -> Set[String]())

  def friendsOf(person: String): Set[String] = network(person)

  def isFriendOf(person: String, friend: String): Boolean = friendsOf(person).contains(friend)

  def unregister(person: String): SocialMedia = {

    @tailrec
    def cleanup(socialMedia: SocialMedia, friends: Set[String]): SocialMedia =
      if (friends.isEmpty) socialMedia
      else cleanup(unfriend(person, friends.head), friends.tail)

    cleanup(this, friendsOf(person)) - person
  }

  def friend(person: String, friend: String): SocialMedia = {

    def addFriend(socialMedia: SocialMedia, person: String, friend: String): SocialMedia =
      socialMedia + (person -> (socialMedia.friendsOf(person) + friend))

    if (isFriendOf(person, friend)) throw new IllegalArgumentException("Already friends")
    else addFriend(addFriend(this, person, friend), friend, person)
  }

  def unfriend(person: String, friend: String): SocialMedia = {

    def removeFriend(socialMedia: SocialMedia, person: String, friend: String): SocialMedia =
      socialMedia + (person -> socialMedia.friendsOf(person).filter(_ != friend))

    if (!isFriendOf(person, friend)) throw new IllegalArgumentException("Not friends")
    else removeFriend(removeFriend(this, person, friend), friend, person)
  }

  def areConnected(person: String, otherPerson: String): Boolean = {

    def areConnected(person: String, otherPerson: String, checked: List[String]): Boolean =
      isFriendOf(person, otherPerson) ||
        friendsOf(person)
          .filter(!checked.contains(_))
          .exists(friend => areConnected(friend, otherPerson, checked :+ person))

    areConnected(person, otherPerson, List())
  }

  def peopleWithMaxFriends(): List[String] = network
    .groupBy(_._2.size)
    .maxBy(_._1)
    ._2
    .toList
    .map(_._1)

  def peopleWithNoFriends(): List[String] = network
    .filter(_._2.isEmpty)
    .toList
    .map(_._1)

  override def toString: String =
    s"""
      |--------- Network ---------
      |${network.mkString("\n")}
      |---------------------------
      |""".stripMargin
}

object SocialMediaTest extends App {

  val ahmed = "Ahmed"
  val khalifa = "Khalifa"
  val mostafa = "Mostafa"
  val hassan = "Hassan"

  var socialMedia = SocialMedia() + ahmed + khalifa + mostafa + hassan

  println(socialMedia)

  socialMedia = socialMedia
    .friend(ahmed, khalifa)
    .friend(khalifa, hassan)
  println(socialMedia)

  println(socialMedia.areConnected(ahmed, khalifa))
  println(socialMedia.areConnected(hassan, ahmed))
  println(socialMedia.areConnected(khalifa, mostafa))
  println(socialMedia)

  println(socialMedia.peopleWithNoFriends())
  println(socialMedia.peopleWithMaxFriends())
  socialMedia = socialMedia.friend(hassan, ahmed)
  println(socialMedia.peopleWithMaxFriends())
  println(socialMedia)
}
