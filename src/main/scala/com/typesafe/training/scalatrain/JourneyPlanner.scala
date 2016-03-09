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

}
