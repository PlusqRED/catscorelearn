package com.grape.catseffect

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object IOs {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }
  object MyIO {
    def putStr(s: => String): MyIO[Unit] =
      MyIO(() => println(s))
  }

  def main(args: Array[String]): Unit = {
    val clock: MyIO[Long] =
      MyIO(() => System.currentTimeMillis())
    def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] = {
      for {
        start <- clock
        res <- action
        end <- clock
      } yield (FiniteDuration(end - start, TimeUnit.MILLISECONDS), res)
    }

    val timedHello = time(MyIO.putStr("hello"))

    timedHello.unsafeRun() match {
      case (duration, _) => println(s"'hello' took $duration")
    }
  }

}
