package com.typesafe.training.scalatrain

import org.joda.time.LocalDate


case class Hop(from: Station, to: Station, train: Train) {
  require(train.stations.contains(from), s"$train should contain station: $from")
  require(train.stations.contains(to), s"$train should contain station: $to")

  val departureTime: Time = train.timeAt(from).get
  val arrivalTime: Time = train.timeAt(to).get

  lazy val price: Currency = train.pricePerHop

  lazy val distance: Double = from.position.distanceTo(to.position)

  def containsStation(station: Station): Boolean =
    from == station || to == station

  def departureIsLaterThan(time: Time): Boolean = departureTime >= time

  def isAvailableGivenDate(date: LocalDate): Boolean = train.isAvailableGivenDate(date)
}
