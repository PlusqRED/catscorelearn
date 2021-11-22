package com.grape.catseffect

import cats.{FlatMap, Monad}
import cats.effect._
import cats.effect.kernel.Resource
import cats.effect.std.CountDownLatch
import cats.syntax.parallel._
import cats.syntax.traverse._
import com.grape.utils.Utils.Util

import java.io.{File, FileWriter}
import scala.concurrent.duration._
import scala.io.Source

object CountdownLatches extends IOApp.Simple {

  /*
    CDLatches are a coordination primitive initialized with a count.
    All fibers calling await() on the CDLatch are (semantically) blocked.
    When the internal count of the latch reaches 0 (via release() calls from other fibers), all waiting fibers are unblocked.
   */

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release // gun firing
    _ <- IO("GO GO GO!").debug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug
    _ <- latch.await // block this fiber until the count reaches 0
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  def sprint(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    _ <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    _ <- announcerFib.join
  } yield ()

  /** Exercise: simulate a file downloader on multiple threads
    */
  object FileServer {
    val fileChunksList: Array[String] = Array(
      "I love Scala.",
      "Cats Effect seems quite fun.",
      "Never would I have thought I would do low-level concurrency WITH pure FP."
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource =
      Resource.make(IO(new FileWriter(new File(path))))(writer =>
        IO(writer.close())
      )
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source =>
        IO(source.close())
      )
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(
        writer => IO(writer.close())
      )
    } yield (reader, writer)

    compositeResource.use { case (reader, writer) =>
      IO(reader.getLines().foreach(content => writer.write(s"$content\n")))
    }
  }

  def downloadingTask(
      chunkId: Int,
      cdLatch: DefaultCDLatch[IO],
      destFolder: String
  ): IO[String] =
    for {
      _ <- IO(s"[Task-$chunkId] starts working...").debug
      chunk <- IO.sleep(1.second) >> FileServer.getFileChunk(chunkId)
      _ <- IO(s"[Task-$chunkId] has loaded chunk: $chunk!").debug
      _ <- writeToFile(s"$destFolder/$chunkId", chunk)
      _ <- cdLatch.release
    } yield chunk

  def startDownloading(
      numOfChunks: Int,
      cdLatch: DefaultCDLatch[IO],
      destFolder: String,
      chunkId: Int = 0
  ): IO[Unit] = (for {
    id <- IO(chunkId)
    _ <- downloadingTask(id, cdLatch, destFolder).start
  } yield id + 1).flatMap(id =>
    if (id < numOfChunks) startDownloading(numOfChunks, cdLatch, destFolder, id)
    else IO.unit
  )

  def collectFile(
      filename: String,
      destFolder: String,
      numOfChunks: Int
  ): IO[Unit] = {
    for {
      _ <- (0 until numOfChunks).toList.traverse(chunkId =>
        appendFileContents(s"$destFolder/$chunkId", s"$destFolder/$filename")
      )
    } yield ()
  }

  /*
            - call file server API and get the number of chunks (n)
            - start a CDLatch
            - start n fibers which download a chunk of the file (use the file server's download chunk API)
            - block on the latch until each task has finished
            - after all chunks are done, stitch the files together under the same file on disk
   */
  import cats.implicits._
  def downloadFile(filename: String, destFolder: String): IO[Unit] = {
    for {
      numOfChunks <- FileServer.getNumChunks
      cdLatch <- DefaultCDLatch[IO](numOfChunks)
      _ <- startDownloading(numOfChunks, cdLatch, destFolder)
      _ <- cdLatch.await
      _ <- collectFile(filename, destFolder, numOfChunks)
      _ <- IO(s"File has been created! Path: $destFolder/$filename").debug
    } yield ()
  }

  trait MyCountDownLatch[F[_]] {
    def release: F[Unit]
    def await: F[Unit]
  }

  protected class DefaultCDLatch[F[_]: Concurrent](
      counter: Ref[F, Int],
      signal: Deferred[F, Unit]
  ) extends MyCountDownLatch[F] {

    override def release: F[Unit] =
      for {
        count <- counter.updateAndGet(_ - 1)
        _ <- if (count == 0) signal.complete(()) else Concurrent[F].unit
      } yield ()

    override def await: F[Unit] = for {
      _ <- signal.get
    } yield ()
  }

  object DefaultCDLatch {
    def apply[F[_]: Concurrent](
        n: Int = 1
    ): F[DefaultCDLatch[F]] =
      for {
        counter <- Concurrent[F].ref[Int](n)
        signal <- Concurrent[F].deferred[Unit]
        cdLatch <- Concurrent[F].pure(new DefaultCDLatch[F](counter, signal))
      } yield cdLatch
  }

  override def run: IO[Unit] =
    downloadFile("my_file.txt", "D:/Projects/catscorelearn/temp")
}
