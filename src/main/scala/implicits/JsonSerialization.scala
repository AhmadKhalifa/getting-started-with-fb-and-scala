package org.scala.learn
package implicits

import implicits.Base._

import java.util.Date

object Base {
  case class User(name: String, email: String, age: Int)

  case class Post(content: String, creationDate: Date)

  case class Feed(user: User, posts: List[Post])

  sealed trait JsonValue {
    def stringify: String
  }

  final case class JsonString(value: String) extends JsonValue {
    override def stringify: String = s"\"$value\""
  }

  final case class JsonInt(value: Int) extends JsonValue {
    override def stringify: String = value.toString
  }

  final case class JsonArray(list: List[JsonValue]) extends JsonValue {
    override def stringify: String = list.map(_.stringify).mkString("[ ", ", ", " ]")
  }

  final case class JsonDate(value: Date) extends JsonValue {
    override def stringify: String = s"\"${value.toString}\""
  }

  final case class JsonObject(obj: Map[String, JsonValue]) extends JsonValue {
    override def stringify: String = obj
      .map(field => s"\"${field._1}\": ${field._2.stringify}")
      .mkString("{ ", ", ", " }")
  }

  val feed = Feed(
    User("Khalifa", "ak@domain.com", 26),
    List(
      Post("First post", new Date(System.currentTimeMillis() - 200000)),
      Post("Second post", new Date(System.currentTimeMillis() - 100000))
    )
  )
}

object JsonSerialization extends App {

  println(
    JsonObject(Map(
      "user" -> JsonObject(Map(
        "name" -> JsonString("Ahmed"),
        "age" -> JsonInt(26)
      )),
      "posts" -> JsonArray(List(
        JsonObject(Map(
          "id" -> JsonInt(1),
          "content" -> JsonString("First post")
        )),
        JsonObject(Map(
          "id" -> JsonInt(2),
          "content" -> JsonString("Second post")
        ))
      ))
    )).stringify
  )

  trait ToJsonValueEnricher {
    def toJsonValue: JsonValue
  }

  implicit class IntJsonEnricher(val int: Int) extends JsonValue with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonInt(int)
  }

  implicit class DateJsonEnricher(val date: Date) extends JsonValue with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonDate(date)
  }

  implicit class StringJsonEnricher(val string: String) extends JsonValue with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonString(string)
  }

  implicit class UserJsonEnricher(val user: User) extends JsonValue with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonObject(Map(
      "name" -> user.name.toJsonValue,
      "email" -> user.email.toJsonValue,
      "age" -> user.age.toJsonValue,
    ))
  }

  implicit class PostJsonEnricher(val post: Post) extends JsonValue with ToJsonValueEnricher  {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonObject(Map(
      "content" -> post.content.toJsonValue,
      "creationDate" -> post.creationDate.toJsonValue
    ))
  }

  implicit class PostsListJsonEnricher(list: List[Post]) extends JsonValue with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonArray(list.map(_.toJsonValue))
  }

  implicit class FeedJsonEnricher(val feed: Feed) extends JsonValue  with ToJsonValueEnricher {
    override def stringify: String = toJsonValue.stringify

    override def toJsonValue: JsonValue = JsonObject(Map(
      "user" -> feed.user.toJsonValue,
      "posts" -> feed.posts.toJsonValue
    ))
  }


  println(feed.stringify)
}

object JsonSerialization2 extends App {

  trait JsonConverter[T] {
    def convert(value: T): JsonValue
    def apply(value: T): JsonValue = convert(value)
  }

  implicit class Json[T](value: T) {
    def toJson(implicit converter: JsonConverter[T]): JsonValue = converter.convert(value)
  }

  implicit object JsonIntConverter extends JsonConverter[Int] {
    override def convert(value: Int): JsonValue = JsonInt(value)
  }

  implicit object JsonStringConverter extends JsonConverter[String] {
    override def convert(value: String): JsonValue = JsonString(value)
  }

  implicit object JsonDateConverter extends JsonConverter[Date] {
    override def convert(value: Date): JsonValue = JsonDate(value)
  }

  trait JsonObjectConverter[T] extends JsonConverter[T]

  trait JsonArrayConverter[T] extends JsonConverter[List[T]]

  implicit object PostsJsonArrayConverter extends JsonArrayConverter[Post] {
    override def convert(posts: List[Post]): JsonValue = JsonArray(posts.map(_.toJson))
  }

  implicit object JsonUserConverter extends JsonObjectConverter[User] {
    override def convert(user: User): JsonValue = JsonObject(Map(
      "name" -> user.name.toJson,
      "email" -> user.email.toJson,
      "age" -> user.age.toJson,
    ))
  }

  implicit object JsonPostConverter extends JsonObjectConverter[Post] {
    override def convert(post: Post): JsonValue = JsonObject(Map(
      "content" -> post.content.toJson,
      "creationDate" -> post.creationDate.toJson,
    ))
  }

  implicit object JsonFeedConverter extends JsonObjectConverter[Feed] {
    override def convert(feed: Feed): JsonValue = JsonObject(Map(
      "user" -> feed.user.toJson,
      "posts" -> feed.posts.toJson
    ))
  }

  trait ToJsonEnricher {
    def toJson: JsonValue
    def stringify: String = toJson.stringify
  }

  implicit class UserToJsonEnricher(user: User) extends ToJsonEnricher {
    override def toJson: JsonValue = Json(user).toJson
  }

  implicit class PostToJsonEnricher(post: Post) extends ToJsonEnricher {
    override def toJson: JsonValue = Json(post).toJson
  }

  implicit class FeedToJsonEnricher(feed: Feed) extends ToJsonEnricher {
    override def toJson: JsonValue = Json(feed).toJson
  }

  implicit class PostsToJsonEnricher(posts: List[Post]) extends ToJsonEnricher {
    override def toJson: JsonValue = Json(posts).toJson
  }

  println(feed.toJson)
  println(feed.stringify)
}
