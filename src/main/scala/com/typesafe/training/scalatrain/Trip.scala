package com.typesafe.training.scalatrain

import org.joda.time.{DateTime, LocalDate, LocalTime, Period}

import scala.util.control.NonFatal

case class Trip private (hops: Seq[Hop]) {

  require(hops.nonEmpty, "A trip must have at lease one hop")
  require(!this.containsCycle, "trips cannot contain cycle")

  import Trip._

  private val lastHop = hops.last
  private val firstHop = hops.head

  lazy val lastStation: Station = lastHop.to
  lazy val lastStationArrivalTime: Time = lastHop.arrivalTime
  lazy val firstStation: Station = firstHop.from
  lazy val tripStartTime: Time = firstHop.departureTime

  lazy val totalCost: Currency = hops.foldLeft(Currency(0))(_ + _.price)
  lazy val totalTime = Time.fromMinutes(lastStationArrivalTime - tripStartTime)

  lazy val containsCycle: Boolean = hops match {
    case head +: Nil => false
    case init :+ lastHop => init.exists(_.containsStation(lastHop.to))
  }

  def appendHop(nextHop: Hop): Option[Trip] = try {
    Some(Trip(hops :+ nextHop))
  } catch { case NonFatal(e) => None}

  def createBooking(departureTime: LocalDate): Booking = Booking(this, departureTime)
}

object Trip {

  def createTrip(hops: Seq[Hop]): Option[Trip] = try {
    Some(Trip(hops))
  } catch { case NonFatal(e) => None}

  case class Booking private[Trip] (trip: Trip, departureDate: LocalDate, bookingDate: DateTime = DateTime.now) {
    private val departureTime = trip.tripStartTime
    val departureDateTime: DateTime = departureDate.toDateTime(new LocalTime(departureTime.hours, departureTime.minutes))

    private val diffInDays = new Period(bookingDate, departureDateTime).toStandardDays.getDays

    val costModifier: Double =
      if (diffInDays >= 14) 1.0
      else if (diffInDays < 1) 0.75
      else 1.5

    val tripCost: Currency = trip.totalCost * costModifier
  }
}