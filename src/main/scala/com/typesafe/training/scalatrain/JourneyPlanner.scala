package com.typesafe.training.scalatrain

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

  def getPossibleTrips(from: Station, to: Station, time: Time): Set[Seq[Hop]] = {

    def getPossibleTripsRec(start: Station, end: Station, startTime: Time, seenStations: Set[Station]): Set[Seq[Hop]] = {
      if (start == end)
        Set(Seq.empty)
      else if(seenStations(start)) {
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

  def getPossibleTrips1(from: Station, to: Station, time: Time): Set[Seq[Hop]] = {
    def getPossibleTripsRec(tripInfo: Set[(Seq[Hop], Time)], validTrips: Set[Seq[Hop]]): Set[Seq[Hop]] = {

      val (completeTripInfo, inCompleteTripInfo) = tripInfo
        .filter{case (trip, _) => hopsByStation.contains(trip.last.to)} //filter out any leaf stations that's not the end of the trip
        .filter{case (trip, _) => //filter out any cycles
          val lastStation = trip.last.to
          !trip.init.exists(hop => hop.from == lastStation || hop.to == lastStation)
        }.partition { case (trip, _) =>
          val lastStation = trip.last.to
          lastStation == to
        }

      def getNextHops(first: Station, latestArrivalTime: Time): Set[Hop] = for {
        (departureStation, _) <- departureTimes if departureStation == first
        hops <- hopsByStation.get(departureStation).toSeq
        hop <- hops if hop.departureTime >= latestArrivalTime
      } yield hop


      if (inCompleteTripInfo.isEmpty) validTrips ++ completeTripInfo.map(_._1)
      else {
        val newTrips: Set[(Seq[Hop], Time)] = inCompleteTripInfo.flatMap { case (trip, latestArrivalTime) =>
          val last = trip.last
          val allNextHops: Set[Hop] = getNextHops(last.to, latestArrivalTime)
          allNextHops.map(hop => (trip :+ hop) -> hop.arrivalTime)
        }

        getPossibleTripsRec(newTrips, validTrips ++ completeTripInfo.map(_._1))
      }
    }

    val firstHops = hopsByStation(from).toSeq

    getPossibleTripsRec(Set(firstHops -> time), Set.empty)
  }

}
