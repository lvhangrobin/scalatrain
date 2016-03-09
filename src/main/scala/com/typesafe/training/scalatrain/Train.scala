package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)], pricePerHop: Currency) {
  require(schedule.size >= 2, "schedule must have at least two stations")
  //TODO verify schedule is increasing in time

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] =
    departureTimes.get(station)
//    schedule.collectFirst { case (time, `station`) => time }

  val backToBackStations: Seq[(Station, Station)] =
    stations.zip(stations.tail)

  val departureTimes: Map[Station, Time] = schedule.map(_.swap).toMap
}
