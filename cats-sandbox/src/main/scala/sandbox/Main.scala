package sandbox

import cats.syntax.all._
import cats.Show
import cats.Eq
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

final case class Cat(name: String, age: Int, color: String)

object Main extends App {
  implicit val catShow: Show[Cat] =
    Show.show(cat => cat.name + " is a " + cat.age + " year-old " + cat.color + " cat.")

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] {
      (xCat, yCat) =>
        xCat.name === yCat.name &&
        xCat.age === yCat.age &&
        xCat.color === yCat.color

    }

  val xCat = Cat(name = "Martina", age = 4, color = "Brown")
  val yCat = Cat(name = "Rosita", age = 2, color = "White")

  val optionXCat = Option(xCat);
  val optionCatEmpty = Option.empty[Cat]

  println(xCat === yCat)
  println(xCat === xCat)
  println(optionXCat =!= optionCatEmpty)
  println(optionXCat === optionXCat)

println(Add.add( List(1, 2) ) )
  //println(Add.add( List( List(1), List(2) ) ))

  println(Add.add( List(Order(5000, 3), Order(10000, 3)) )(Add.orderMonoid) )


}
