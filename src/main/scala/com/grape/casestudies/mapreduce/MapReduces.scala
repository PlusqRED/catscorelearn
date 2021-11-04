package com.grape.casestudies.mapreduce

import cats.Monoid

import scala.concurrent.Future

object MapReduces {
  import cats.syntax.semigroup._

  def foldMap[A, B: Monoid](vector: Vector[A])(f: A => B): B =
    vector.map(f).reduce(_ |+| _)

  def parallelFoldMap[A, B: Monoid](
      values: Vector[A]
  )(func: A => B): Future[B] = {
    import cats.instances.vector._
    import cats.syntax.foldable._
    import cats.syntax.traverse._

    import scala.concurrent.ExecutionContext.Implicits.global
    val coreAmount = Runtime.getRuntime.availableProcessors()
    val groupSize = values.size / coreAmount;
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  def main(args: Array[String]): Unit = {}
}
