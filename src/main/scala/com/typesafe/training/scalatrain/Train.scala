package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays.WeekDay

import scala.collection.immutable.Seq
import org.joda.time.DateTime

case class Train(
  info: TrainInfo,
  pricePerHop: Currency,
  schedule: Seq[(Time, Station)],
  recurringCalendar: Set[WeekDay],
  exceptionalCalendar: Set[DateTime] = Set.empty
  ) {
  require(schedule.size >= 2, "schedule must have at least two stations")
  //TODO verify schedule is increasing in time

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] =
    departureTimes.get(station)
//    schedule.collectFirst { case (time, `station`) => time }

  val backToBackStations: Seq[(Station, Station)] =
    stations.zip(stations.tail)

  val departureTimes: Map[Station, Time] = schedule.map(_.swap).toMap

  def isAvailableGivenDay(day: WeekDay): Boolean = recurringCalendar(day)

  def isAvailableGivenDate(date: DateTime): Boolean =
    ! exceptionalCalendar(date) &&
      isAvailableGivenDay(getDayGivenDate(date))

  private def getDayGivenDate(date: DateTime): WeekDay = {
    WeekDays.getById(date.dayOfWeek.get - 1)
  }
}
