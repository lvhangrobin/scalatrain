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

  val stations: Seq[Station] = schedule.routine.map(_._2)

  def timeAt(station: Station): Option[Time] =
    departureTimes.get(station)

  val backToBackStations: Seq[(Station, Station)] =
    stations.zip(stations.tail)

  val departureTimes: Map[Station, Time] = schedule.routine.map(_.swap).toMap

  def isAvailableGivenDay(day: WeekDay): Boolean = schedule.runsOnDay(day)

  def isAvailableGivenDate(date: LocalDate): Boolean = schedule.runsOnDate(date)
}
