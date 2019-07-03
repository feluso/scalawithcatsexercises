package sandbox

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

import cats.Functor

object Tree {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

    implicit val treeFunctor: Functor[Tree] =
      new Functor[Tree] {
        def map[A, B](value: Tree[A])(func: A => B): Tree[B] =
          value match {
            case Branch(left, right) => Branch(map(left)(func), map(right)(func))
            case Leaf(x) => Leaf(func(x))
          }
      }
}

trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
}

object Printable {
def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

implicit val booleanPrintable: Printable[Boolean] =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] =
    printable.contramap((box: Box[A]) => box.value)
}


final case class Box[A](value: A)

trait Codec[A] {
  self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))
      def decode(value: String): B =
        dec(self.decode(value))
    }
}

object Codec {
    def encode[A](value: A)(implicit c: Codec[A]): String =
      c.encode(value)

    def decode[A](value: String)(implicit c: Codec[A]): A =
      c.decode(value)

    implicit val stringCodec: Codec[String] =
      new Codec[String] {
        def encode(value: String): String = value
        def decode(value: String): String = value
      }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)

    implicit val booleanCodec: Codec[Boolean] =
      stringCodec.imap(_.toBoolean, _.toString)

    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap(_.toDouble, _.toString)

    implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
      codec.imap(Box(_), _.value)
}
