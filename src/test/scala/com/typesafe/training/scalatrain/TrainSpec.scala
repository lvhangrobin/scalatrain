/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.TestData._
import org.joda.time.{DateTimeZone, DateTime}
import java.lang.{ IllegalArgumentException => IAE }
import org.scalatest.{ Matchers, WordSpec }

class TrainSpec extends WordSpec with Matchers {

  "Train ice724" should {
    "stop in Nurember" in {
      ice724.timeAt(nuremberg) shouldEqual Some(ice724NurembergTime)
    }
    "not stop in Essen" in {
      ice724.timeAt(essen) shouldEqual None
    }
  }

  "Train ice726" should {
    "stop in Munich" in {
      ice726.timeAt(munich) shouldEqual Some(ice726MunichTime)
    }
    "not stop in Cologne" in {
      ice726.timeAt(cologne) shouldEqual None
    }
  }

  "Creating a Train" should {
    "throw an IllegalArgumentException for a schedule with 0 or 1 elements" in {
      an[IAE] should be thrownBy
        Train(InterCityExpress(724), defaultPrice, Vector(), defaultRecurring)
      an[IAE] should be thrownBy
        Train(InterCityExpress(724), defaultPrice, Vector(ice724MunichTime -> munich), defaultRecurring)
    }
  }

  "stations" should {
    "be initialized correctly" in {
      ice724.stations shouldEqual Vector(munich, nuremberg, frankfurt, cologne)
    }
  }

  "back to back stations" should {
    "work properly" in {
      ice724.backToBackStations shouldEqual
        Seq(
          munich -> nuremberg,
          nuremberg -> frankfurt,
          frankfurt -> cologne
        )
    }
  }

  "departure times" should {
    "swap the elements in schedule" in {
      ice724.departureTimes shouldEqual
        Map(
          munich -> ice724MunichTime,
          nuremberg -> ice724NurembergTime,
          frankfurt -> ice724FrankfurtTime,
          cologne -> ice724CologneTime
        )
    }
  }

  "trains with recurring and exceptional calendars" should {
    val exceptionalDate = new DateTime(2016, 3, 9, 0, 0, DateTimeZone.UTC)
    val sunday = new DateTime(2016, 3, 13, 0, 0, DateTimeZone.UTC)
    val tuesday = new DateTime(2016, 3, 8, 0, 0, DateTimeZone.UTC)
    val testTrain = train1.copy(exceptionalCalendar =
      Set(exceptionalDate)) // Wednesday
    "identify that a train is available on a recurring day" in {
      testTrain.isAvailableGivenDate(tuesday) shouldBe true
    }
    "identify that a train is unavailable on a non-recurring day" in {
      testTrain.isAvailableGivenDate(sunday) shouldBe false
    }
    "identify that a train is unavailable on an exception day" in {
      testTrain.isAvailableGivenDate(exceptionalDate) shouldBe false
    }
  }
}
