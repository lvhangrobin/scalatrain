package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.WeekDays._
import org.joda.time.{DateTime, LocalDate}

import scala.annotation.tailrec
import scala.collection.immutable.Seq


case class Schedule(
  routine: Seq[(Time, Station)],
  recurringCalendar: Set[WeekDay],
  exceptionalCalendar: Set[LocalDate] = Set.empty
){

  def runningDaysSince(date: LocalDate): Int = {
    val now: LocalDate = LocalDate.now
    val maintenanceDate = date

    @tailrec
    def runningDaysSinceRec(date: LocalDate, acc: Int): Int = {
      val day = getDayGivenDate(date)
      val newAcc =
        if(recurringCalendar(day) && ! exceptionalCalendar(date))
          acc + 1
        else
          acc

      if(date == now) newAcc else runningDaysSinceRec(date.plusDays(1),newAcc)
    }
    runningDaysSinceRec(maintenanceDate.plusDays(1), 0)
  }

  def runsOnDay(day: WeekDay): Boolean = recurringCalendar(day)

  def runsOnDate(date: LocalDate): Boolean =
    ! exceptionalCalendar(date) &&
      runsOnDay(getDayGivenDate(date))

  private def getDayGivenDate(date: LocalDate): WeekDay = {
    WeekDays.getById(date.dayOfWeek.get - 1)
  }
}
