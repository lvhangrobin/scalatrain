package com.typesafe.training.scalatrain

case class Currency (priceCents: Int) extends Ordered[Currency]{

  require(priceCents >= 0, "priceCents")

  lazy val toDollars: Double = priceCents/100D

  private def - (that: Currency): Int =
    this.priceCents - that.priceCents

  def + (that: Currency): Currency =
    Currency(this.priceCents + that.priceCents)

  def * (constant: Double): Currency =
    Currency((this.priceCents * constant).toInt)

  override def compare(that: Currency): Int =
    this - that
}
