package com.typesafe.training.scalatrain

import org.scalatest.{Matchers, WordSpec}
import TestData._

class CurrencySpec extends WordSpec with Matchers {

  "Currency" should {
    "correctly convert from cents to dollars" in {
      defaultPrice.toDollars shouldEqual 50.00
    }
  }

}
