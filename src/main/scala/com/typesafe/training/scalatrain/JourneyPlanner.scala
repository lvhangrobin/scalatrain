package com.typesafe.training.scalatrain

import org.joda.time.LocalDate

import scala.annotation.tailrec

class JourneyPlanner(trains: Set[Train]) {

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

  def getPossibleTripsRegardlessOfTime(from: Station, to: Station, date: LocalDate): Set[Trip] = {
    getPossibleTrips(from, to, date, Time(0))
  }

  def getPossibleTrips(from: Station, to: Station, date: LocalDate, minDepartTime: Time): Set[Trip] = {

    @tailrec
    def getPossibleTripsRec(trips: Set[Trip], validTrips: Set[Trip]): Set[Trip] = {

      def getNextHops(first: Station, latestArrivalTime: Time): Set[Hop] =
        for {
          (departureStation, _) <- departureTimes if departureStation == first
          potentialNextHops <- hopsByStation.get(departureStation).toSeq
          validNextHop <- potentialNextHops
          if validNextHop.departureTime >= latestArrivalTime && validNextHop.train.isAvailableGivenDate(date)
        } yield validNextHop

      def filterOutLeafStations(trip: Trip): Boolean =
        hopsByStation.contains(trip.lastStation)

      val (completeTrips, inCompleteTrips) = {
        val (complete, inComplete) = trips.partition(_.lastStation == to)

        complete -> inComplete.filter(trip => filterOutLeafStations(trip))
      }

      if (inCompleteTrips.isEmpty)
        validTrips ++ completeTrips
      else {
        val newTrips: Set[Trip] = for {
          trip <- inCompleteTrips
          validNextHops = getNextHops(trip.lastStation, trip.lastStationArrivalTime)
          validHop <- validNextHops
          validTrip <- trip.appendHop(validHop)
        } yield validTrip

        getPossibleTripsRec(newTrips, validTrips ++ completeTrips)
      }
    }

    val firstHops =
      hopsByStation(from)
        .filter(hop => hop.departureIsLaterThan(minDepartTime) && hop.isAvailableGivenDate(date))
        .map(hop => Trip(Seq(hop)))

    getPossibleTripsRec(firstHops, Set.empty)
  }

  def scheduledTrainsByDate(date: LocalDate): Set[Train] =
    trains.filter(_.isAvailableGivenDate(date))

}

object JourneyPlanner {

  def totalCostForTrip(trip: Trip) = trip.totalCost

  def sortByTotalTravelTime(trips: Set[Trip]): Seq[Trip] = trips.toSeq.sortBy(_.totalTime)
  
  def sortByTotalCost(trips: Set[Trip]): Seq[Trip] = trips.toSeq.sortBy(_.totalCost)
}

