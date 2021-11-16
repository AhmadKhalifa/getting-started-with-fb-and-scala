package org.scala.learn
package types

object PathIndependent extends App {

  trait ItemLike {
    type Type
  }

  trait Item[T] extends ItemLike {
    final override type Type = T
  }

  class StringItem extends Item[String]
  class IntItem extends Item[Int]

  def get[T <: ItemLike](value: T#Type): String = ???

  get[StringItem]("")
//  get[StringItem](1)

  get[IntItem](12)
//  get[IntItem]("")
}
