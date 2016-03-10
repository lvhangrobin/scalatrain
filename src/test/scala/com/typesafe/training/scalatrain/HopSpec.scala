package com.typesafe.training.scalatrain

import org.scalatest.{Matchers, WordSpec}
import com.typesafe.training.scalatrain.TestData._

class HopSpec extends WordSpec with Matchers {

  "the distance between 2 stations" should {
    "be calculated" in {
      val hop = Hop(stationA, stationB, train1)

      hop.distance shouldEqual 5.0
    }

  }
}
