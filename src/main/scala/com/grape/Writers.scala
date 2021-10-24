package com.grape

import cats.data.Writer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Writers {
  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  import cats.syntax.applicative._
  import cats.syntax.writer._

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorialWriter(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged] else factorialWriter(n - 1).map(_ * n)
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def main(args: Array[String]): Unit = {
//    val (logs, value) = factorialWriter(5).run
//    println(logs, value)

    println(
      Await.result(
        Future
          .sequence(
            Vector(
              Future(factorialWriter(5)),
              Future(factorialWriter(5))
            )
          )
          .map(vector => vector.map(logged => logged.written)),
        5.seconds
      )
    )
  }

}
