package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.TestData._
import org.joda.time.LocalDate
import org.scalatest.{Matchers, WordSpec}


class TripSpec extends WordSpec with Matchers{

  "Calling createBooking" should {

    val trip = Trip.createTrip(Seq(Hop(stationA, stationB, train1), Hop(stationB, stationD, train1))).get

    "return discount value within one day" in {
      val departureDate = LocalDate.now
      trip.createBooking(departureDate).tripCost shouldEqual Currency(7500)
    }

    "return regular value if booked two weeks in advance" in {
      val departureDate = LocalDate.now.plusWeeks(3)
      trip.createBooking(departureDate).tripCost shouldEqual Currency(10000)
    }

    "return 150% value if booked within two weeks" in {
      val departureDate = LocalDate.now.plusDays(10)
      trip.createBooking(departureDate).tripCost shouldEqual Currency(15000)
    }
  }

  "Calling createTrip" should {
    "return None if hops are empty" in {
      Trip.createTrip(Nil) shouldBe None
    }

    "return None if cycle exists" in {
      val cycledHops = Seq(Hop(stationA, stationB, train1), Hop(stationB, stationA, train8))
      Trip.createTrip(cycledHops) shouldBe None
    }
  }
}
