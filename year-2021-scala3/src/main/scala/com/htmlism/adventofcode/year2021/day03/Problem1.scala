package com.htmlism.adventofcode
package year2021.day03

import cats.data.NonEmptyList

object Problem1:
  def solve(xs: NonEmptyList[String]): String =
    xs
      .map { binaryStr =>
        val bits =
          binaryStr.split("")

        bits
          .map {
            case "0" =>
              BitBucket(1, 0)

            case "1" =>
              BitBucket(0, 1)

            case _ =>
              throw new IllegalArgumentException("unexpected bit value")
          }: Array[BitBucket]
      }
      .reduceLeft { (xs, ys) =>
        xs
          .zip(ys)
          .map { case (x, y) =>
            BitBucket(x.zeroes + y.zeroes, x.ones + y.ones)
          }
      }
      .toString
