/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import TestData._
import java.lang.{ IllegalArgumentException => IAE }
import org.scalatest.{ Matchers, WordSpec }

class JourneyPlannerSpec extends WordSpec with Matchers {

  "stations" should {
    "be initialized correctly" in {
      planner.stations shouldEqual Set(munich, nuremberg, frankfurt, cologne, essen)
    }
  }

  "Calling trainsAt" should {
    "return the correct trains" in {
      planner.trainsAt(munich) shouldEqual Set(ice724, ice726)
      planner.trainsAt(cologne) shouldEqual Set(ice724)
    }
  }

  "Calling stopsAt" should {
    "return the correct stops" in {
      planner.stopsAt(munich) shouldEqual Set(ice724MunichTime -> ice724, ice726MunichTime -> ice726)
    }
  }

  "Calling isShortTrip" should {
    "return false for more than one station in between" in {
      planner.isShortTrip(munich, cologne) shouldBe false
      planner.isShortTrip(munich, essen) shouldBe false
    }
    "return true for zero or one stations in between" in {
      planner.isShortTrip(munich, nuremberg) shouldBe true
      planner.isShortTrip(munich, frankfurt) shouldBe true
      planner.isShortTrip(nuremberg, frankfurt) shouldBe true
      planner.isShortTrip(nuremberg, essen) shouldBe true
    }
  }

  "Calling getPossibleTrips" should {
//    "1" in {
//      myPlanner.getPossibleTrips(stationA, stationD, time10) should contain theSameElementsAs
//        Set(
//          Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1)),
//          Seq(Hop(stationA, stationC, train2), Hop(stationC, stationB, train2), Hop(stationB, stationD, train2)),
//          Seq(Hop(stationA, stationC, train3), Hop(stationC, stationD, train3))
//        )
//    }

    "2" in {
      val expected = Set(
        Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1)),
        Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train2)),
        Seq(Hop(stationA, stationC, train2), Hop(stationC, stationB, train2), Hop(stationB, stationD, train2))
      )
      myPlanner.getPossibleTrips(stationA, stationD, time10) should contain theSameElementsAs expected
    }


  }
}
