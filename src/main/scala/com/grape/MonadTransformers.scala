package com.grape

import cats.data.EitherT

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MonadTransformers {
//  type Response[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  import cats.syntax.applicative._

  import scala.concurrent.ExecutionContext.Implicits.global
  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None =>
        EitherT.left(Future(s"$autobot level is unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLvl1 <- getPowerLevel(ally1)
      powerLvl2 <- getPowerLevel(ally2)
    } yield (powerLvl1 + powerLvl2) > 15

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(value)  => s"Error! $value"
      case Right(true)  => "Special move applied!"
      case Right(false) => "Special move failed!"
    }
  }

  def main(args: Array[String]): Unit = {
    val powerLvl: EitherT[Future, String, Int] = for {
      powerLevel <- getPowerLevel("Jazzz")
    } yield powerLevel
    println(powerLvl.value)

    println(tacticalReport("Jazz", "Jazzz"))
  }
}
