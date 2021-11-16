package org.scala.learn
package implicits

case class Purchase(numberOfItems: Int, pricePerUnit: Double)

object Purchase {
  implicit val totalOrdering: Ordering[Purchase] =
    Ordering[Double] on ((purchase: Purchase) => purchase.numberOfItems * purchase.pricePerUnit)

  object NumberOfItemsOrdering {
    implicit val ordering: Ordering[Purchase] = Ordering[Int] on[Purchase] (_.numberOfItems)
  }

  object PricePerUnitOrdering {
    implicit val ordering: Ordering[Purchase] = Ordering[Double] on[Purchase] (_.pricePerUnit)
  }
}



object PurchaseTest extends App {

  import implicits.Purchase.PricePerUnitOrdering._

  List(
    Purchase(1, 40),
    Purchase(5, 40),
    Purchase(10, 1),
  ).sorted foreach println
}