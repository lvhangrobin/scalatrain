package com.typesafe.training.scalatrain

case class Trip(hops: Seq[Hop]) {
  require(hops.nonEmpty, "A trip must have at lease one hop")

  private val lastHop = hops.last
  private val firstHop = hops.head

  val lastStation: Station = lastHop.to
  val lastStationArrivalTime: Time = lastHop.arrivalTime
  val firstStation: Station = firstHop.from
  val tripStartTime: Time = firstHop.departureTime

  val totalCost: Currency = hops.foldLeft(Currency(0))(_ + _.price)

  def appendHop(nextHop: Hop): Trip = Trip(hops :+ nextHop)
}
