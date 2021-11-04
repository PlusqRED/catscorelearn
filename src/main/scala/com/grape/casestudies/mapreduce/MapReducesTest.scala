package com.grape.casestudies.mapreduce

import com.grape.casestudies.mapreduce.MapReduces.parallelFoldMap
import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.concurrent.Await
import scala.concurrent.duration._

class MapReducesTest extends AnyFlatSpec with should.Matchers {

  "parallelFoldMap" should "work on int vectors" in {
    assert(
      Await.result(
        parallelFoldMap((1 to 100).toVector)(_ * 3),
        3.second
      ) == 15150
    )
  }
}
