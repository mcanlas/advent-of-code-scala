package com.htmlism.adventofcode.year2021.day03

object BitBucket {
  val zero: BitBucket =
    BitBucket(0, 0)
}

case class BitBucket(zeroes: Int, ones: Int)
