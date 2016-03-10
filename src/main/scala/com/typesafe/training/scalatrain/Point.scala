package com.typesafe.training.scalatrain

import com.typesafe.training.scalatrain.Point.Kilometer

import scala.math.{sqrt, pow}

case class Point(x: Kilometer, y: Kilometer) {

  def distanceTo(other: Point): Kilometer =
    sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))

}

object Point {
  type Kilometer = Double
}
