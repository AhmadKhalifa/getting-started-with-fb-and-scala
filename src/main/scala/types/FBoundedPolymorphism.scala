package org.scala.learn
package types

object FBoundedPolymorphism extends App {

  trait Animal[T] {
    def breed(animal: T): List[T]
  }

  class Dog
  object Dog {
    implicit object DogAnimal extends Animal[Dog] {
      override def breed(animal: Dog): List[Dog] = new Dog :: new Dog :: Nil
    }
  }

  class Cat
  object Cat {
    implicit object CatAnimal extends Animal[Cat] {
      override def breed(animal: Cat): List[Cat] = new Cat :: Nil
    }
  }

  implicit class CanBreedOps[T](animal: T) {
    def breed(implicit animal: Animal[T]): List[T] = animal.breed(this.animal)
  }

  (new Cat).breed foreach println
  (new Dog).breed foreach println
}
