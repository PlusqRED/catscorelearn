package com.grape

import cats.{Eq, Functor, Show}
import com.grape.Playground.Printable

object Maps {
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  def main(args: Array[String]): Unit = {
    val str: String = encode(Box(3))
    val option: Option[Box[Int]] =
      Functor[Option].imap(Option(2))(Box(_))(_.value)
    val option1: Option[Box[Int]] = Functor[Option].map(Option(2))(Box(_))
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  final case class Cat(name: String, age: Int, color: String)

  final case class Box[A](value: A)

  object Cat {
    implicit val catPrintable: Printable[Cat] = (a: Cat) =>
      s"${a.name} is a ${a.age} year-old ${a.color} cat."

    implicit val catShow: Show[Cat] = (t: Cat) => catPrintable.format(t)

    implicit val catEq: Eq[Cat] = (x: Cat, y: Cat) =>
      x.name.equals(y.name) && x.color.equals(y.color) && x.age.equals(y.age)
  }

  import cats.instances.option._

  object Codec {
    implicit def boxCodecIntances[A](implicit
        codec: Codec[A]
    ): Codec[Box[A]] = {
      new Codec[Box[A]] {
        override def encode(value: Box[A]): String = value.value.toString

        override def decode(value: String): Box[A] = Box(codec.decode(value))
      }
    }

    implicit val intCodec: Codec[Int] = new Codec[Int] {
      override def encode(value: Int): String = value.toString

      override def decode(value: String): Int = value.toInt
    }
  }
}
