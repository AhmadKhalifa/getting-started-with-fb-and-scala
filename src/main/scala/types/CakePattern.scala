package org.scala.learn
package types

object CakePattern extends App {

  trait Repository {
    def findById(id: Long): String
  }

  trait Service { repository: Repository =>
    def getById(id: Long): String = findById(id)
  }

  trait Controller { service: Service =>
    def getResource(id: Long): String = getById(id)
  }

  ////////////////////////

  trait UserRepository extends Repository
  trait ProductRepository extends Repository

  trait UserService extends Service with UserRepository
  trait ProductService extends Service with ProductRepository

  trait UserController extends Controller with UserService
  trait ProductController extends Controller with ProductService

  val userController: UserController = (id: Long) => id.toString

  println(userController.getResource(1))
}
