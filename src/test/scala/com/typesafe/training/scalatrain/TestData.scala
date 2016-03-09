/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

object TestData {

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
    Vector(
      ice724MunichTime -> munich,
      ice724NurembergTime -> nuremberg,
      ice724FrankfurtTime -> frankfurt,
      ice724CologneTime -> cologne
    )
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
    Vector(
      ice726MunichTime -> munich,
      ice726NurembergTime -> nuremberg,
      ice726FrankfurtTime -> frankfurt,
      ice726CologneTime -> essen
    )
  )

  val train1 = Train(
    InterCityExpress(1),
    Vector(
      time10 -> stationA,
      time11 -> stationB,
      time12 -> stationD
    )
  )

  val train2 = Train(
    InterCityExpress(2),
    Vector(
      time11 -> stationA,
      time12 -> stationC,
      time13 -> stationB,
      time14 -> stationD
    )
  )

  val train3 = Train(
    InterCityExpress(3),
    Vector(
      time12 -> stationA,
      time13 -> stationC,
      time14 -> stationD
    )
  )

  val train4 = Train(
    InterCityExpress(4),
    Vector(
      time13 -> stationA,
      time14 -> stationC,
      time15 -> stationE
    )
  )

  val train5 = Train(
    InterCityExpress(4),
    Vector(
      time15 -> stationE,
      time16 -> stationB,
      time17 -> stationA
    )
  )

  val planner = new JourneyPlanner(Set(ice724, ice726))
  val myPlanner = new JourneyPlanner(Set(train1, train2))
}
