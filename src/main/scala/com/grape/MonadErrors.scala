package com.grape

import cats.MonadError

import scala.util.Try

object MonadErrors {
  def main(args: Array[String]): Unit = {
    println(validateAdult[Try](18))
    // res7: Try[Int] = Success(18)
    println(validateAdult[Try](8))
    // res8: Try[Int] = Failure(
    // java.lang.IllegalArgumentException: Age must be greater than or
    //equal to 18
    // )
    type ExceptionOr[A] = Either[Throwable, A]
    println(validateAdult[ExceptionOr](-1))
    // res9: ExceptionOr[Int] = Left(
    // java.lang.IllegalArgumentException: Age must be greater than or
    //equal to 18
    // )
  }

  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] =
    me.ensure(me.pure(age))(
      new IllegalArgumentException("Age must be greater than or equal to 18")
    )(_ >= 18)
}
