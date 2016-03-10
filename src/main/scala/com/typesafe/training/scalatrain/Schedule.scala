package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays._
import org.joda.time.LocalDate

import scala.collection.immutable.Seq


case class Schedule(
  routine: Seq[(Time, Station)],
  recurringCalendar: Set[WeekDay],
  exceptionalCalendar: Set[LocalDate] = Set.empty
){
  def runsOnDay(day: WeekDay): Boolean = recurringCalendar(day)

  def runsOnDate(date: LocalDate): Boolean =
    ! exceptionalCalendar(date) &&
      runsOnDay(getDayGivenDate(date))

  private def getDayGivenDate(date: LocalDate): WeekDay = {
    WeekDays.getById(date.dayOfWeek.get - 1)
  }
}
