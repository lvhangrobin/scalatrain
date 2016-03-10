package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays.WeekDay

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import org.joda.time.{Period, Interval, LocalDate}

case class Train(
  info: TrainInfo,
  pricePerHop: Currency,
  lastMaintenanceDate: LocalDate,
  schedule: Schedule,
  maxMaintenanceDistance: Int = 100000,
  maxMaintenanceTime: Period = new Period(365L * 24 * 60 * 60 * 1000) // 1 year
) {
  require(schedule.routine.size >= 2, "schedule must have at least two stations")
  //TODO verify schedule is increasing in time

  lazy val stations: Seq[Station] = schedule.routine.map(_._2)

  lazy val backToBackStations: Seq[(Station, Station)] =
    stations.zip(stations.tail)

  lazy val departureTimes: Map[Station, Time] = schedule.routine.map(_.swap).toMap

  lazy val totalDistancePerDay: Double =
    backToBackStations.foldLeft(0D){ case(acc, (from, to)) => acc + from.position.distanceTo(to.position) }

  lazy val totalDistanceSinceLastMaintenance: Double =
    schedule.runningDaysSince(lastMaintenanceDate) * totalDistancePerDay

  def nextMaintenanceDay: LocalDate = {
    val potentialNextMaintenanceDay = nextMaintenanceDayUntil(maxMaintenanceDistance)
    val yearFromLastMaintenance = lastMaintenanceDate.plus(maxMaintenanceTime.toStandardDays)

    if (potentialNextMaintenanceDay.isBefore(yearFromLastMaintenance)) potentialNextMaintenanceDay
    else yearFromLastMaintenance
  }

  private def nextMaintenanceDayUntil(distance: Double): LocalDate = {
    val runningDays: Int = (distance / totalDistancePerDay).toInt

    @tailrec
    def nextMaintenanceDayUntilRec(date: LocalDate, remainingDays: Int): LocalDate = {
      val newRemainingDays =
        if (schedule.runsOnDate(date)) remainingDays - 1
        else remainingDays

      if (remainingDays == 0) date
      else nextMaintenanceDayUntilRec(date.plusDays(1), newRemainingDays)
    }

    nextMaintenanceDayUntilRec(lastMaintenanceDate.plusDays(1), runningDays)
  }

  def timeAt(station: Station): Option[Time] =
    departureTimes.get(station)

  def isAvailableGivenDay(day: WeekDay): Boolean = schedule.runsOnDay(day)

  def isAvailableGivenDate(date: LocalDate): Boolean = schedule.runsOnDate(date)
}
