package com.grape

import cats.Monad

import scala.annotation.tailrec

object CustomMonads {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Leaf(a)             => f(a)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }
    }

    def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
          open: List[Tree[Either[A, B]]],
          closed: List[Option[Tree[B]]]
      ): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }
      loop(List(func(arg)), Nil).head
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }
}
