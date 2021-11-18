package com.grape.catseffect

import cats.Id
import cats.effect.kernel.Ref
import cats.effect.{Deferred, IO, IOApp}
import cats.syntax.parallel._
import com.grape.utils.Utils.Util

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Mutexes extends IOApp.Simple {
  abstract class Mutex {
    def acquire: IO[Unit]
    def release: IO[Unit]
  }

  object Mutex {
    def create: IO[Mutex] = for {
      lock <- Ref[IO].of(true)
      refPipe <- Ref.ofEffect(Deferred[IO, Unit])
      mutex <- IO(new Mutex {
        override def acquire: IO[Unit] = for {
          key <- lock.getAndSet(false)
          pipe <- refPipe.get
          _ <- if (key) IO.unit else pipe.get *> acquire
        } yield ()

        override def release: IO[Unit] = for {
          pipe <- refPipe.get
          _ <- lock.set(true) *> pipe.complete(()) *> Deferred[IO, Unit]
            .flatMap(refPipe.set)
        } yield ()
      })
    } yield mutex
  }

  def criticalTask(): IO[Int] = IO.sleep(3.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id got result: $res").debug
  } yield res

  def demoNonLockingTask(): IO[List[Int]] =
    (1 to 3).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"[task $id] waiting for permission...").debug
    _ <- mutex.acquire
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result: $res").debug
    _ <- mutex.release
    _ <- IO(s"[task $id] lock removed.").debug
  } yield res

  def demoLockingTasks(): IO[List[Int]] = for {
    mutex <- Mutex.create
    results <- (1 to 3).toList.parTraverse(id => createLockingTask(id, mutex))
  } yield results

  override def run: IO[Unit] = for {
    res <- demoLockingTasks()
    _ <- IO(res).debug
  } yield ()
}
