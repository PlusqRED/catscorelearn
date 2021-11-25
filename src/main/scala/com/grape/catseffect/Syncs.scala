package com.grape.catseffect

import cats.effect.{IO, IOApp, Sync}

import scala.io.StdIn

object Syncs extends IOApp.Simple {

  trait Console[F[_]] {
    def println[A](a: A): F[Unit]

    def readLine(): F[String]
  }

  object Console {
    def apply[F[_]: Sync]: F[Console[F]] = Sync[F].delay(new Console[F] {
      override def println[A](a: A): F[Unit] =
        Sync[F].blocking(Predef.println(a))

      override def readLine(): F[String] = Sync[F].blocking(StdIn.readLine())
    })
  }

  override def run: IO[Unit] = for {
    console <- Console[IO]
    name <- console.readLine()
    _ <- console.println(s"Hello $name")
  } yield ()
}
