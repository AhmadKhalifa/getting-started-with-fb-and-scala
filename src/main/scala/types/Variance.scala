package org.scala.learn
package types

object Variance extends App {

  trait Animal

  trait WildAnimal extends Animal
  trait Pet extends Animal

  class Horse extends WildAnimal

  class Hamster extends Pet

  class Cat extends Pet
  class Kitten extends Cat

  class Dog extends Pet
  class Puppy extends Dog

  /**
   * Reference: https://www.udemy.com/course/advanced-scala/learn/lecture/11053858
   * Variance can be simplified by this question: "Can we consider a cage of cats as a cage of animals?!"
   * answer match {
   *    case "NO. it's just a cage of cats" => Invariance
   *    case "Yes" => Covariance
   *    case "ABSOLUTELY NOT! (OPPOSITE). We should consider the cage of animals as a cage of cats" => Contravariance
   * }
   *
   * As a rule or thumb: method arguments are in contravariant position while method return types are covariant
   */

  /* --------------------------------------------- */

  /**
   * Invariance means that a cage of cats "IS NOTHING BUT" a cage of cats
   * val catsCage: InvariantCage[Cat] = new InvariantCage[Cat]
   */

  class InvariantCage[T](val animal: T)
  class BigInvariantCage[T](val animals: List[T] = Nil) {

    // INVALID. Think of this example if the code compiles
    // val animalsCage: BigInvariantCage[Animal] = new BigInvariantCage[Dog]
    // animals.cage(new Cat)
//    def add(animal: T): BigInvariantCage[T] =
//      new BigCovariantCage[T](animals += animal)

    def add(animal: T): BigInvariantCage[T] =
      new BigInvariantCage[T](animal +: animals)                              // Same type
  }

  // A kitten cage is NOT a sub-type of a cat cage
  val catInvariantCage: InvariantCage[Cat] =                                  // Cat cage
//  new InvariantCage[Kitten](new Kitten)                                     // INVALID, Kitten cage
//  new InvariantCage[Cat](new Kitten)                                        // Valid, Still a cat cage even if a kitten in
  new InvariantCage[Cat](new Cat)                                             // Cat cage


  // A cage of (cats/pets) "IS A" cage of animals
  val catsBigInvariantCage: BigInvariantCage[Cat] =                           // Cage of animals
    new BigInvariantCage[Cat]()                                               // Cage of cats
      .add(new Cat)                                                           // Cage of cats
      .add(new Kitten)                                                        // VALID, Still a cat cage even if a kitten in

  /* --------------------------------------------- */

  /**
   * Covariance means that a cage of cats "IS A" cage of animals
   * val animalsCage: Cage[Animal] = new Cage[Cat]
   *
   * Note: animal MUST be 'val' not 'var'. Think of this example if the code compiles
   * val animalsCage: Cage[Animal] = new Cage[Cat]
   * animalsCage.add(new Horse)
   * Which doesn't make sense
   */
  class CovariantCage[+T](val animal: T)

  class BigCovariantCage[+T](val animals: List[T] = Nil) {
    def add[S >: T](animal: S): BigCovariantCage[S] =
      new BigCovariantCage[S](animal +: animals) // Widening the type
  }

  // A cage of cat is a sub-type of a cage of animals
  val catCovariantCage: CovariantCage[Animal] =                               // Animal cage
    new CovariantCage[Cat](new Cat)                                           // Cat cage

  // A cage of (cats/pets) is a sub-type of a cage of animals
  val petsBigCovariantCage: BigCovariantCage[Animal] =                        // Cage of animals
    new BigCovariantCage[Cat]()                                               // Cage of cats
      .add(new Cat)                                                           // Cage of cats
      .add(new Kitten)                                                        // Cage of cats
      .add(new Dog)                                                           // Cage of pets

  // A list of kittens can be a cage of cats
  // BUT a cage of pets is not a sub-type of a cage of cats
  val catsBigCovariantCage: BigCovariantCage[Cat] =                           // Cage of cats
    new BigCovariantCage[Cat]()
      .add(new Kitten)                                                        // Cage of cats
      .add(new Cat)                                                           // Cage of cats
//      .add(new Dog)                                                         // INVALID, Cage of pets

  /* --------------------------------------------- */

  /**
   * Contravariance means that a cage of cats "IS NOT A" cage of animals. But a cage of animals "IS A" cage of cats.
   * Like if a hamster cage can't fit animals in it. But any animal cage can fit a hamster in.
   * Doesn't make sense? Think of it as an animal trainer. If John can train animals, then John is an horse trainer
   * class Trainer[-T]
   * val trainer: Trainer[Horse] = new Trainer[Animal]
   */

  // INVALID. Think of this example if the code compiles
  // val catCage: ContravariantCage[Cat] = new ContravariantCage[Animal](new Horse /* extends Animal */)
  // We cant put a horse in a cat cage of course
  // Same goes for var instead of val
//  class ContravariantCage[-T](val animal: T)
//  class ContravariantCage[-T](var animal: T)

  class Shop[-T] {
    def get[S <: T](isBaby: Boolean, defaultPet: S): S = defaultPet
  }

  val dogsShop: Shop[Dog] = new Shop[Pet]

//  dogsShop.get(isBaby = false, new Cat)                                     // INVALID, passed type must be a Dog or any sub-class of Dog
  dogsShop.get(isBaby = false, new Dog)                                       // VALID, Dog type
  dogsShop.get(isBaby = true, new Puppy)                                      // VALID, A Dog Sub-class type
}
