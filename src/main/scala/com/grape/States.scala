package com.grape

import cats.data.State

object States {

  type CalcState[A] = State[List[Int], A]

  def operator(function: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val answer = function(a, b)
        (answer :: tail, answer)
      case _ => sys.error("Fail!")
    }

  def operand(num: Int): CalcState[Int] = {
    State[List[Int], Int](stack => (num :: stack, num))
  }

  def evalOne(sym: String): CalcState[Int] = sym.trim match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] = input match {
    case head :: tail => evalOne(head).flatMap(_ => evalAll(tail))
    case Nil          => State[List[Int], Int](stack => (stack, stack.head))
  }

  import cats.syntax.applicative._
  def evalAll2(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int = {
    evalAll(input.split(',').toList).runA(Nil).value
  }

  def main(args: Array[String]): Unit = {
    val program = for {
      _ <- evalOne("3")
      _ <- evalOne("2")
      ans <- evalOne("*")
    } yield ans
    println(program.runA(Nil).value)

    println(evalAll2(List("2", "5", "+", "3", "/", "3", "*")).run(Nil).value)

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    // biggerProgram: cats.data.IndexedStateT[cats.Eval, List[Int], List[
    //    Int], Int] = cats.data.IndexedStateT@2c2c50d1
    println(biggerProgram.runA(Nil).value)
    // res14: Int = 21

    println(evalInput("1,2,+,3,4,+,*"))
  }
}
