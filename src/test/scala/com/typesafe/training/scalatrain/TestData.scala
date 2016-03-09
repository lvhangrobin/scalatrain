/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays.WeekDay
import com.typesafe.training.scalatrain.WeekDays._
import org.joda.time.{DateTimeZone, DateTime}

object TestData {

  val defaultPrice: Currency = Currency(50 * 100)
  val defaultRecurring: Set[WeekDay] = Set(Monday, Tuesday, Wednesday, Thursday, Friday)
  val defaultDay: DateTime = new DateTime(2016, 3, 9, 0, 0, DateTimeZone.UTC)

  val munich = Station("Munich")

  val nuremberg = Station("Nuremberg")

  val frankfurt = Station("Frankfurt")

  val cologne = Station("Cologne")

  val essen = Station("Essen")

  val ice724MunichTime = Time(8, 50)

  val ice724NurembergTime = Time(10)

  val ice724FrankfurtTime = Time(12, 10)

  val ice724CologneTime = Time(13, 39)

  val ice726MunichTime = Time(7, 50)

  val ice726NurembergTime = Time(9)

  val ice726FrankfurtTime = Time(11, 10)

  val ice726CologneTime = Time(13, 2)

  val ice724 = Train(
    InterCityExpress(724),
    defaultPrice,
    Vector(
      ice724MunichTime -> munich,
      ice724NurembergTime -> nuremberg,
      ice724FrankfurtTime -> frankfurt,
      ice724CologneTime -> cologne
    ),
    defaultRecurring
  )

  val time10 = Time(10)
  val time11 = Time(11)
  val time12 = Time(12)
  val time13 = Time(13)
  val time14 = Time(14)
  val time15 = Time(15)
  val time16 = Time(16)
  val time17 = Time(17)

  val stationA = Station("A")
  val stationB = Station("B")
  val stationC = Station("C")
  val stationD = Station("D")
  val stationE = Station("E")

  val ice726 = Train(
    InterCityExpress(726),
    defaultPrice,
    Vector(
      ice726MunichTime -> munich,
      ice726NurembergTime -> nuremberg,
      ice726FrankfurtTime -> frankfurt,
      ice726CologneTime -> essen
    ),
    defaultRecurring
  )

  val train1 = Train(
    InterCityExpress(1),
    defaultPrice,
    Vector(
      time10 -> stationA,
      time11 -> stationB,
      time12 -> stationD
    ),
    defaultRecurring
  )

  val train2 = Train(
    InterCityExpress(2),
    defaultPrice,
    Vector(
      time11 -> stationA,
      time12 -> stationC,
      time13 -> stationB,
      time14 -> stationD
    ),
    defaultRecurring
  )

  val train3 = Train(
    InterCityExpress(3),
    defaultPrice,
    Vector(
      time12 -> stationA,
      time13 -> stationC,
      time14 -> stationD
    ),
    defaultRecurring
  )

  val train4 = Train(
    InterCityExpress(4),
    defaultPrice,
    Vector(
      time10 -> stationA,
      time14 -> stationC,
      time15 -> stationE
    ),
    defaultRecurring
  )

  val train5 = Train(
    InterCityExpress(5),
    defaultPrice,
    Vector(
      time15 -> stationE,
      time16 -> stationB,
      time17 -> stationA
    ),
    defaultRecurring
  )

  val train6 = Train(
    InterCityExpress(6),
    defaultPrice,
    Vector(
      time10 -> stationA,
      time12 -> stationB,
      time13 -> stationD
    ),
    defaultRecurring
  )

  val train7 = Train(
    InterCityExpress(7),
    defaultPrice,
    Vector(
      time12 -> stationA,
      time14 -> stationB,
      time15 -> stationD
    ),
    defaultRecurring
  )

  val train8 = Train(
    InterCityExpress(8),
    defaultPrice,
    Vector(
      time11 -> stationB,
      time12 -> stationA
    ),
    defaultRecurring
  )

  val planner = new JourneyPlanner(Set(ice724, ice726))
  def createPlanner(trains: Set[Train]) = new JourneyPlanner(trains)
}
