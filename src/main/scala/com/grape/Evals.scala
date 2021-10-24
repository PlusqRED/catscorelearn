package com.grape

import cats.Eval

object Evals {
  // not stack safe
  /*  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }*/

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, Eval.defer(Eval.later(foldRight(tail, acc)(fn))).value)
      case Nil =>
        acc
    }

}
