/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import TestData._
import org.joda.time.{LocalDate, DateTimeZone, DateTime}
import java.lang.{IllegalArgumentException => IAE}
import org.scalatest.{Matchers, WordSpec}
import com.typesafe.training.scalatrain.WeekDays.Sunday

class JourneyPlannerSpec extends WordSpec with Matchers {

  val journeyPlannerCycles = createPlanner(Set(train1, train7, train8))
  val journeyPlannerSimple = createPlanner(Set(train1, train6))
  val journeyPlannerComplex = createPlanner(Set(train1, train2, train3, train4, train5))

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
    "properly extract valid trips" in {
      journeyPlannerComplex.getPossibleTrips(stationA, stationD, defaultDay, time10) should contain theSameElementsAs
        Set(
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1))),
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train2))),
          Trip(Seq(Hop(stationA, stationC, train2), Hop(stationC, stationB, train2), Hop(stationB, stationD, train2))),
          Trip(Seq(Hop(stationA, stationC, train3), Hop(stationC, stationD, train3))),
          Trip(Seq(Hop(stationA, stationC, train2), Hop(stationC, stationD, train3)))
        )
    }

    "allow valid trains to leave at the same time" in {
      journeyPlannerSimple.getPossibleTrips(stationA, stationD, defaultDay, time10) should contain theSameElementsAs
        Set(
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1))),
          Trip(Seq(Hop(stationA, stationB, train6), Hop(stationB, stationD, train6))),
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train6)))
        )
    }

    "eliminate trips which contain cycles" in {
      journeyPlannerCycles.getPossibleTrips(stationA, stationD, defaultDay, time10) should contain theSameElementsAs
        Set(
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1))),
          Trip(Seq(Hop(stationA, stationB, train7), Hop(stationB, stationD, train7))),
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train7)))
        )
    }
  }

  "Calling sortByTotalTravelTime" should {
    "correctly sort in ascending order" in {
      val possibleTrips = journeyPlannerCycles.getPossibleTrips(stationA, stationD, defaultDay, time10)

      JourneyPlanner.sortByTotalTravelTime(possibleTrips) shouldEqual
        Seq(
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1))),
          Trip(Seq(Hop(stationA, stationB, train7), Hop(stationB, stationD, train7))),
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train7)))
        )
    }
  }

  "Calling sortByTotalCost" should {
    "correctly sort in ascending order" in {
      val cheapTrain = train1.copy(pricePerHop = Currency(30 * 100))
      val regularTrain = train7

      val possibleTrips =
        createPlanner(Set(cheapTrain, regularTrain)).getPossibleTrips(stationA, stationD, defaultDay, time10)

      JourneyPlanner.sortByTotalCost(possibleTrips) shouldEqual
        Seq(
          Trip(Seq(Hop(stationA, stationB, cheapTrain), Hop(stationB, stationD, cheapTrain))), // $30 * 2
          Trip(Seq(Hop(stationA, stationB, cheapTrain), Hop(stationB, stationD, regularTrain))), // $30 + $50
          Trip(Seq(Hop(stationA, stationB, regularTrain), Hop(stationB, stationD, regularTrain))) // $50 * 2
        )

    }
  }

  "Calling getPossibleTripsRegardlessOfTime" should {
    "return the correct trips" in {
      val rescheduledTrain6 = train6.copy(schedule =
        train6.schedule.copy(recurringCalendar = Set(Sunday)))
      val newJourney = createPlanner(Set(train1, rescheduledTrain6))
      newJourney.getPossibleTripsRegardlessOfTime(stationA, stationD, defaultDay) shouldBe
        Set(
          Trip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1)))
        )
    }
  }

  "sinkStations" should {
    "only contains stations don't have next hops" in {
      journeyPlannerSimple.sinkStations shouldEqual Set(stationD)
    }
  }
}