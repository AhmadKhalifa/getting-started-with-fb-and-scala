package org.scala.learn
package types

object ParkingSystem extends App {

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  class IList[T]

  class InvariantParkingSystem[T](vehicles: List[T]) {
    def park(vehicle: T): InvariantParkingSystem[T] = ???
    def impound(vehicles: List[T]): InvariantParkingSystem[T] = ???
    def checkVehicles(): List[T] = ???
    def flatMap(transformer: T => InvariantParkingSystem[T]): InvariantParkingSystem[T] = ???
  }

  class CovariantParkingSystem[+T](vehicles: List[T]) {
    def park[S >: T](vehicle: S): CovariantParkingSystem[S] = ???
    def impound[S >: T](vehicles: List[S]): CovariantParkingSystem[S] = ???
    def checkVehicles(): List[T] = ???
    def flatMap[S >: T](transformer: T => CovariantParkingSystem[S]): CovariantParkingSystem[S] = ???
  }

  class ContravariantParkingSystem[-T](vehicles: List[T]) {
    def park(vehicle: T): ContravariantParkingSystem[T] = ???
    def impound(vehicles: List[T]): ContravariantParkingSystem[T] = ???
    def checkVehicles[S <: T](): List[S] = ???
    def flatMap[S <: T, R](transformer: R => ContravariantParkingSystem[S]): ContravariantParkingSystem[T] = ???
  }

  //////////////

  class CovariantParkingSystemWithInvariantList[+T](vehicles: IList[T]) {
    def park[S >: T](vehicle: S): CovariantParkingSystemWithInvariantList[S] = ???
    def impound[S >: T](vehicles: IList[S]): CovariantParkingSystemWithInvariantList[S] = ???
    def checkVehicles[S >: T](): IList[S] = ???
    def flatMap[S >: T](transformer: T => CovariantParkingSystemWithInvariantList[S]): CovariantParkingSystemWithInvariantList[S] = ???
  }

  class ContravariantParkingSystemWithInvariantList[-T](vehicles: IList[T]) {
    def park(vehicle: T): ContravariantParkingSystemWithInvariantList[T] = ???
    def impound[S <: T](vehicles: IList[S]): ContravariantParkingSystemWithInvariantList[T] = ???
    def checkVehicles[S <: T](): IList[S] = ???
    def flatMap[S <: T, R](transformer: R => ContravariantParkingSystemWithInvariantList[S]): ContravariantParkingSystemWithInvariantList[T] = ???
  }
}
