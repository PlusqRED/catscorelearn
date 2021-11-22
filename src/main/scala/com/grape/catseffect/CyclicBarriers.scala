package com.grape.catseffect

import cats.effect.kernel.Concurrent
import cats.effect.{IO, IOApp}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._
import com.grape.utils.Utils.Util

import scala.concurrent.duration.DurationInt
import scala.util.Random

object CyclicBarriers extends IOApp.Simple {

  def createUser(id: Int, barrier: MyCyclicBarrier[IO]): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble() * 500).toInt.millis)
    _ <- IO(
      s"[user $id] Just heard there's a new social network - signing up for the waitlist..."
    ).debug
    _ <- IO.sleep((Random.nextDouble() * 1500).toInt.millis)
    _ <- IO(s"[user $id] On the waitlist now, can't wait!").debug
    _ <- barrier.await
    _ <- IO(s"[user $id] OMG this is so cool!").debug
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("[announcer] Launching when we have 5 users!").debug
    barrier <- MyCyclicBarrier[IO](5)
    _ <- (1 to 12).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  trait MyCyclicBarrier[F[_]] {
    def await: F[Unit]
  }

  object MyCyclicBarrier {
    def apply[F[_]: Concurrent](capacity: Int): F[MyCyclicBarrier[F]] = for {
      counter <- Concurrent[F].ref[Int](0)
      signalRef <- Concurrent[F].deferred[Unit].flatMap(Concurrent[F].ref)
    } yield new MyCyclicBarrier[F] {
      override def await: F[Unit] = for {
        count <- counter.updateAndGet(_ + 1)
        currentSignal <- signalRef.get
        _ <-
          if (count >= capacity)
            currentSignal.complete(()) >> Concurrent[F]
              .deferred[Unit]
              .flatMap(signalRef.set) >> counter.update(_ - capacity)
          else currentSignal.get
      } yield ()
    }
  }

  override def run: IO[Unit] = openNetwork()
}
