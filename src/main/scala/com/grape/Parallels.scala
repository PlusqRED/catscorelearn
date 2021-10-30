package com.grape

import cats.Semigroupal
import cats.implicits._

object Parallels {
  def main(args: Array[String]): Unit = {
    type ErrorOr[T] = Either[List[String], T]

    val err1: ErrorOr[Int] = List("error1").asLeft[Int]
    val err2: ErrorOr[Int] = List("error2").asLeft[Int]

    // if we want to persist all errors from some monads we have executed use: par... methods
    // use Semigroupal or Applicative for independent execution and Monads for sequenced
    val value1: ErrorOr[Int] = (err1, err2).parMapN((a, b) => a + b)
    println(value1)
    val value2: ErrorOr[(Int, Int)] = (err1, err2).parTupled
    println(value2)
  }
}
