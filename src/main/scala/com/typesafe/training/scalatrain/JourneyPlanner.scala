package com.typesafe.training.scalatrain

import org.joda.time.DateTime

import scala.annotation.tailrec

class JourneyPlanner(trains: Set[Train]) {

  import JourneyPlanner._

  lazy val stations: Set[Station] = trains.flatMap(_.stations)

  lazy val hops: Set[Hop] =
    for {
      train <- trains
      (from, to) <- train.backToBackStations
    } yield Hop(from, to, train)

  lazy val hopsByStation: Map[Station, Set[Hop]] = hops.groupBy(_.from)

  lazy val departureTimes: Set[(Station, Time)] =
    trains.flatMap(_.departureTimes)

  def trainsAt(station: Station): Set[Train] = trains.filter(_.stations.contains(station))

  def stopsAt(station: Station): Set[(Time, Train)] =
    for {
      train <- trains
      time <- train.timeAt(station)
    } yield time -> train

  def isShortTrip(from: Station, to: Station): Boolean = {
    trains.exists(train => train.stations.dropWhile(_ != from) match {
      case `from` +: one +: `to` +: tail => true
      case `from` +: `to` +: tail => true
      case _ => false
    })
  }

  private def getPossibleTripsOld(from: Station, to: Station, time: Time): Set[Trip] = {

    def getPossibleTripsRec(start: Station, end: Station, startTime: Time, seenStations: Set[Station]): Set[Trip] = {
      if (start == end)
        Set(Seq.empty)
      else if (seenStations(start)) {
        Set.empty
      } else {
        for {
          (departureStation, _) <- departureTimes if departureStation == start
          hops <- hopsByStation.get(departureStation).toSeq
          hop <- hops if hop.departureTime >= startTime
          trip <- getPossibleTripsRec(hop.to, end, hop.arrivalTime, seenStations + start)
        } yield trip match {
          case Nil => Seq(hop)
          case _ => hop +: trip
        }
      }
    }

    getPossibleTripsRec(from, to, time, Set.empty)
  }

  def getPossibleTrips(from: Station, to: Station, time: Time): Set[Trip] = {

    @tailrec
    def getPossibleTripsRec(tripInfo: Set[(Trip, Time)], validTrips: Set[Trip]): Set[Trip] = {

      def getNextHops(first: Station, latestArrivalTime: Time): Set[Hop] =
        for {
          (departureStation, _) <- departureTimes if departureStation == first
          potentialNextHops <- hopsByStation.get(departureStation).toSeq
          validNextHop <- potentialNextHops if validNextHop.departureTime >= latestArrivalTime
        } yield validNextHop

      def filterOutLeafStations(trip: Trip): Boolean =
        hopsByStation.contains(trip.last.to)

      def filterOutCycles(trip: Trip): Boolean =
        trip match {
          case head +: Nil => true
          case init :+ lastStation => !init.exists(_.containsStation(lastStation.to))
        }

      val (completeTripInfo, inCompleteTripInfo) = {
        val (complete, inComplete) = tripInfo.partition { case (trip, _) => trip.last.to == to }

        complete -> inComplete.filter { case (trip, _) => filterOutLeafStations(trip) && filterOutCycles(trip) }
      }

      if (inCompleteTripInfo.isEmpty)
        validTrips ++ completeTripInfo.map(_._1)
      else {
        val newTrips: Set[(Trip, Time)] = for {
          (trip, latestArrivalTime) <- inCompleteTripInfo
          validNextHops = getNextHops(trip.last.to, latestArrivalTime)
          validHop <- validNextHops
        } yield (trip :+ validHop) -> validHop.arrivalTime

        getPossibleTripsRec(newTrips, validTrips ++ completeTripInfo.map(_._1))
      }
    }

    val firstHops =
      hopsByStation(from)
        .filter(_.departureTime >= time)
        .map(hop => Seq(hop) -> hop.arrivalTime)

    getPossibleTripsRec(firstHops, Set.empty)
  }

  def scheduledTrainsByDate(date: DateTime): Set[Train] = 
    trains.filter(_.isAvailableGivenDate(date))

}

object JourneyPlanner {

  type Trip = Seq[Hop]

  def sortByTotalTravelTime(trips: Set[Trip]): Seq[Trip] = {
    def timeForTrip(trip: Trip): Time = Time.fromMinutes(trip.last.arrivalTime - trip.head.departureTime)

    trips.toSeq.sortBy(trip => timeForTrip(trip))
  }

  def sortByTotalCost(trips: Set[Trip]): Seq[Trip] = {
    def costForTrip(trip: Trip): Currency = trip.foldLeft(Currency(0))(_ + _.price)

    trips.toSeq.sortBy(trip => costForTrip(trip))
  }
}
