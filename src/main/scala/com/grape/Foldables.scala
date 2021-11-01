package com.grape

import cats.{Eval, Foldable, Monoid}

object Foldables {

  implicit class MyList[A](list: List[A]) {
    def myMap[B](f: A => B): List[B] =
      list.foldRight(List.empty[B])((el, acc) => f(el) :: acc)

    def myFlatMap[B](f: A => List[B]): List[B] =
      list.foldRight(List.empty[B])((el, acc) => f(el) ::: acc)

    def myFilter(f: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((el, acc) => if (f(el)) el :: acc else acc)

    def mySum(implicit monoid: Monoid[A]): A = {
      list.foldRight(monoid.empty)(monoid.combine)
    }
  }

  def main(args: Array[String]): Unit = {
    val value: List[Int] =
      Foldable[List]
        .foldRight(List(1, 2, 3, 4, 5), Eval.now(List.empty[Int]))((i, acc) =>
          Eval.defer(Eval.always(i :: acc.value))
        )
        .value
    println(value)

    println(List(1, 2, 3).myMap(_ * 10))
    println(List(1, 2, 3).myFlatMap(el => List(el, 100)))
    println(List("1", "2", "3").myFilter(_.toInt > 2))
    println(List(1, 2, 3).mySum)
  }
}
