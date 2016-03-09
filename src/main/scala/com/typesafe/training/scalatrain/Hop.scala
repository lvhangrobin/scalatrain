package com.typesafe.training.scalatrain


case class Hop(from: Station, to: Station, train: Train) {
  require(train.stations.contains(from), s"$train should contain station: $from")
  require(train.stations.contains(to), s"$train should contain station: $to")

  val departureTime: Time = train.timeAt(from).get
  val arrivalTime: Time = train.timeAt(to).get

  def containsStation(station: Station): Boolean =
    from == station || to == station
}
