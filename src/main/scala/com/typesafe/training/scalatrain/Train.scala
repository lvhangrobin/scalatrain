package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size >= 2, "schedule must have at least two stations")
  //TODO verify schedule is increasing in time

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] =
    schedule.find(_._2 == station).map(_._1)
//    schedule.collectFirst { case (time, `station`) => time }

}
