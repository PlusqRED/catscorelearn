package com.grape

import cats.{Id, Monad}

import scala.annotation.tailrec

object Monads {
  /*  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }*/

  implicit val monadId: Monad[Id] = new Monad[Id] {
    override def pure[A](x: A): Id[A] = x: Id[A]

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa: Id[A])

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(
      a
    ) match {
      case Right(b) => b
      case Left(a)  => tailRecM(a)(f)
    }
  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def main(args: Array[String]): Unit = {
    prtlnInt(sumSquare(3: Id[Int], 5: Id[Int]))
  }

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  def prtlnInt(value: Int): Unit = println(value)
}
