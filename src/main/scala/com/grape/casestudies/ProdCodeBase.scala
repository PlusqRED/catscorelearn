package com.grape.casestudies

import cats.Applicative

object ProdCodeBase {
  import cats.instances.list._
  import cats.syntax.traverse._
  import cats.syntax.functor._

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }
}
