package com.typesafe.training.scalatrain

import play.api.libs.json.{JsValue, Json}

import scala.util.Try

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time]{
  require(hours >= 0 && hours <= 23, "hours must be between 0 and 23")
  require(minutes >= 0 && minutes <= 59, "minutes must be between 0 and 59")

  val asMinutes: Int =
    hours * 60 + minutes

  def minus(that: Time): Int = asMinutes - that.asMinutes

  def -(that: Time): Int = minus(that)

  override lazy val toString: String = f"$hours%02d:$minutes%02d"

  override def compare(that: Time): Int =
    this - that

  def toJson(): JsValue = {
    Json.obj("hours" -> hours, "minutes" -> minutes)
  }
}

object Time {
  def fromMinutes(minutes: Int): Time = Time(minutes / 60, minutes % 60)

  def fromJson(json: JsValue): Option[Time] =
    for {
      h <- Try((json \ "hours").as[Int]).toOption
      m = Try((json \ "minutes").as[Int]).getOrElse(0)
    } yield Time(h, m)
}
