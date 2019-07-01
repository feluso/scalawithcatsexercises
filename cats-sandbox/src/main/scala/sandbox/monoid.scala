package sandbox

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.double._


case class Order(totalCost: Double, quantity: Double)

object Add {


implicit val orderMonoid: Monoid[Order] =
  new Monoid[Order] {
    def combine(a: Order, b: Order): Order =
      Order(totalCost = a.totalCost |+| b.totalCost, quantity = a.quantity |+| b.quantity)
    def empty: Order = Order(0, 0)
  }

  def add[A](items: List[A])(implicit monoid: Monoid[A]) : A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)
}
