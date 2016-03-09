package com.typesafe.training.scalatrain

object WeekDays extends Enumeration {
  type WeekDay = Value
  val Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday = Value

  def getById(id: Int) = WeekDays(id)
}