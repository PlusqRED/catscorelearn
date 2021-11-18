package com.grape.utils

import cats.effect.IO

object Utils {
  implicit class Util[A](io: IO[A]) {
    def debug: IO[A] = for {
      res <- io
      _ <- IO.println(res)
    } yield res
  }
}
