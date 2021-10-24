package com.grape

import cats.data.Reader

object Readers {
  type DbReader[A] = Reader[Db, A]

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    implicit val db = Db(users, passwords)
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      usernameOption <- findUsername(userId)
      isAllowed <- usernameOption
        .map(username => checkPassword(username, password))
        .getOrElse(false.pure[DbReader])
    } yield isAllowed
  }

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  import cats.syntax.applicative._

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

}
