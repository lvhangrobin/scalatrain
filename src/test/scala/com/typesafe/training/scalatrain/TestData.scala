/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays.{WeekDay, _}
import org.joda.time.LocalDate

object TestData {

  val defaultPrice: Currency = Currency(50 * 100)
  val mondayToFridayRecurring: Set[WeekDay] = Set(Monday, Tuesday, Wednesday, Thursday, Friday)
  val fullWeekRecurring: Set[WeekDay] = Set(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
  val defaultDay: LocalDate = new LocalDate(2016, 3, 9)
  val defaultPoint = Point(1,1)
  val defaultLastMaintenanceDate = defaultDay
  val dateNow = LocalDate.now

  val munich = Station("Munich", defaultPoint)

  val nuremberg = Station("Nuremberg", defaultPoint)

  val frankfurt = Station("Frankfurt", defaultPoint)

  val cologne = Station("Cologne", defaultPoint)

  val essen = Station("Essen", defaultPoint)

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
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        ice724MunichTime -> munich,
        ice724NurembergTime -> nuremberg,
        ice724FrankfurtTime -> frankfurt,
        ice724CologneTime -> cologne
      ),
      mondayToFridayRecurring
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

  val stationA = Station("A", Point(1,1))
  val stationB = Station("B", Point(4,5))
  val stationC = Station("C", defaultPoint)
  val stationD = Station("D", Point(7,9))
  val stationE = Station("E", defaultPoint)

  val ice726 = Train(
    InterCityExpress(726),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        ice726MunichTime -> munich,
        ice726NurembergTime -> nuremberg,
        ice726FrankfurtTime -> frankfurt,
        ice726CologneTime -> essen
      ),
      mondayToFridayRecurring
    )
  )

  val train1 = Train(
    InterCityExpress(1),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time10 -> stationA,
        time11 -> stationB,
        time12 -> stationD
      ),
      mondayToFridayRecurring
    )
  )

  val train2 = Train(
    InterCityExpress(2),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time11 -> stationA,
        time12 -> stationC,
        time13 -> stationB,
        time14 -> stationD
      ),
      mondayToFridayRecurring
    )
  )

  val train3 = Train(
    InterCityExpress(3),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time12 -> stationA,
        time13 -> stationC,
        time14 -> stationD
      ),
      mondayToFridayRecurring
    )
  )

  val train4 = Train(
    InterCityExpress(4),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time10 -> stationA,
        time14 -> stationC,
        time15 -> stationE
      ),
      mondayToFridayRecurring
    )
  )

  val train5 = Train(
    InterCityExpress(5),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time15 -> stationE,
        time16 -> stationB,
        time17 -> stationA
      ),
      mondayToFridayRecurring
    )
  )

  val train6 = Train(
    InterCityExpress(6),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time10 -> stationA,
        time12 -> stationB,
        time13 -> stationD
      ),
      mondayToFridayRecurring
    )
  )

  val train7 = Train(
    InterCityExpress(7),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time12 -> stationA,
        time14 -> stationB,
        time15 -> stationD
      ),
      mondayToFridayRecurring
    )
  )

  val train8 = Train(
    InterCityExpress(8),
    defaultPrice,
    defaultLastMaintenanceDate,
    Schedule(
      Vector(
        time11 -> stationB,
        time12 -> stationA
      ),
      mondayToFridayRecurring
    )
  )

  val train9 = Train(
    InterCityExpress(1),
    defaultPrice,
    dateNow.minusDays(7),
    Schedule(
      Vector(
        time10 -> stationA,
        time11 -> stationB,
        time12 -> stationD
      ),
      fullWeekRecurring,
      Set(dateNow.minusDays(1))
    )
  )

  val planner = new JourneyPlanner(Set(ice724, ice726))
  def createPlanner(trains: Set[Train]) = new JourneyPlanner(trains)
}
