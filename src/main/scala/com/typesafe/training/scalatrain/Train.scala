package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays.WeekDay

import scala.collection.immutable.Seq
import org.joda.time.LocalDate

case class Train(
  info: TrainInfo,
  pricePerHop: Currency,
  lastMaintenanceDate: LocalDate,
  schedule: Schedule
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


  def timeAt(station: Station): Option[Time] =
    departureTimes.get(station)

  def isAvailableGivenDay(day: WeekDay): Boolean = schedule.runsOnDay(day)

  def isAvailableGivenDate(date: LocalDate): Boolean = schedule.runsOnDate(date)
}
