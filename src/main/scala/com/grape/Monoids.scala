package com.grape

import cats.Functor
import cats.kernel.Monoid

object Monoids {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.semigroup._

  val worldAppender: List[String] => List[String] =
    Functor[List].lift[String, String](_ + " world")

  def main(args: Array[String]): Unit = {
    println(add(List(Option(1), Option(2), Option(3), Option(4), Option(5))))
    println(add(List(1, 2, 3, 4, 5)))
    println(add(List(Order(2.3d, 5), Order(4.5d, 3))))
    println(worldAppender(List("Hello", "Oleg")))
    println(Branch(Leaf(1), Branch(Leaf(2), Leaf(5))).map(_ * 100))
  }

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  sealed trait Tree[+A]

  case class Order(totalCost: Double, quantity: Double)

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Order {
    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0d, 0d)

      override def combine(x: Order, y: Order): Order =
        Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }
  }

  object Tree {
    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value)         => Leaf(f(value))
      }
    }

    implicit class TreeFunctorSyntax[A](value: Tree[A]) {
      def map[B](f: A => B)(implicit functor: Functor[Tree]): Tree[B] =
        functor.map(value)(f)
    }
  }
}
