package com.grape.utils

import cats.Functor
import cats.effect.MonadCancel
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration
object Utils {
  implicit class Util[F[_]: Functor, A](effect: F[A]) {
    def debug: F[A] = effect.map { value =>
      println(s"[${Thread.currentThread().getName}] - $value")
      value
    }

    def unsafeSleep[E](
        duration: FiniteDuration
    )(implicit mc: MonadCancel[F, E]): F[Unit] = {
      mc.map(effect) { el =>
        Thread.sleep(duration.toMillis)
        el
      }
    }
  }
}
