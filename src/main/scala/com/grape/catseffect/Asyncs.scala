package com.grape.catseffect

import cats.effect.kernel.Concurrent
import cats.effect._

object Asyncs extends IOApp.Simple {

  trait MyAsync[F[_]] {
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]

    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A]

    def never[A]: F[A]
  }

  object MyAsync {
    def apply[F[_]](implicit
        sync: Sync[F],
        temporal: Temporal[F]
    ): F[MyAsync[F]] =
      sync.delay(new MyAsync[F] {
        override def async[A](
            cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]
        ): F[A] = ???

        override def async_[A](
            cb: (Either[Throwable, A] => Unit) => Unit
        ): F[A] =
          async(cb.andThen(_ => sync.pure(None)))

        override def never[A]: F[A] = temporal.never
      })
  }

  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)

  def secondEffect[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] = for {
    el1 <- firstEffect(a)
    el2 <- secondEffect(a)
  } yield (el1, el2)

  override def run: IO[Unit] =
    tupledEffect("hello")(Async[IO]).flatMap(IO.println).void
}
