package com.typesafe.training.scalatrain

case class Currency(priceCents: Int) {

  lazy val toDollars: Double = priceCents/100D

}
