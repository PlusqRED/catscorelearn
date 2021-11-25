package com.grape.catseffect

import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.Concurrent
import cats.effect.{IO, IOApp, MonadCancel}
import com.grape.utils.Utils.Util

import scala.concurrent.duration._

object Cancellations extends IOApp.Simple {

  import cats.syntax.flatMap._
  def inputPassword[F[_], E](implicit mc: MonadCancel[F, E]): F[String] =
    mc.pure("Input password:").debug >> mc
      .pure(
        "(typing password)"
      )
      .debug
      .unsafeSleep(2.seconds) >> mc.pure("RockTheJVM1")

  def verifyPassword[F[_], E](
      pw: String
  )(implicit mc: MonadCancel[F, E]): F[Boolean] = {
    mc
      .pure("verifying...")
      .debug
      .unsafeSleep(2.seconds) >> mc.pure(pw == "RockTheJVM1")
  }

  import cats.effect.syntax.monadCancel._
  import cats.syntax.functor._
  def authFlow[F[_], E](implicit mc: MonadCancel[F, E]): F[Unit] =
    mc.uncancelable { poll =>
      for {
        pw <- poll(inputPassword).onCancel(
          mc
            .pure("Authentication timed out. Try again later.")
            .debug
            .void
        )
        verified <- verifyPassword(pw)
        _ <-
          if (verified) mc.pure("Authentication successful.").debug
          else mc.pure("Authentication failed.").debug
      } yield ()
    }

  def authProgram[F[_], E](implicit
      conc: Concurrent[F]
  ): F[Unit] = for {
    authFib <- authFlow.start
    _ <- conc.pure().unsafeSleep(4.seconds) >> conc
      .pure(
        "Authentication timeout, attempting cancel..."
      )
      .debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] = authProgram[IO, Throwable]
}
