package com.grape

import cats.Show
import cats.effect.IO

object Playground {
  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val printableString: Printable[String] = (a: String) => a

    implicit val printableInt: Printable[Int] = (a: Int) => a.toString
  }

  object Printable {
    def print[A: Show](a: A): Unit = println(Show[A].show(a))
  }

  def main(args: Array[String]): Unit = {}
}
