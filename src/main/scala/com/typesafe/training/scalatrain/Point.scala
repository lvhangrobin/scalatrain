package com.typesafe.training.scalatrain

import scala.math.{sqrt, pow}

case class Point(x: Int, y: Int) {

  def distanceTo(other: Point): Double =
    sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))

}