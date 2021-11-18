package com.grape.catseffect

import cats.effect.{IO, IOApp}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

object PlaygroundIO extends IOApp.Simple {
  val run: IO[Unit] =
    for {
      ctr <- IO.ref(0)

      wait = IO.sleep(1.second)
      poll = wait *> ctr.get

      _ <- poll.flatMap(IO.println(_)).foreverM.start
      _ <- poll.map(_ % 3 == 0).ifM(IO.println("fizz"), IO.unit).foreverM.start
      _ <- poll.map(_ % 5 == 0).ifM(IO.println("buzz"), IO.unit).foreverM.start

      _ <- (wait *> ctr.update(_ + 1)).foreverM.void
    } yield ()
}
