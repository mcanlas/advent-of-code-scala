package com.htmlism.adventofcode
package year2021.day01

import cats.data.NonEmptyList
import cats.syntax.all._

object Problem2:
  def solve(xs: NonEmptyList[String]): String =
    xs
      .map(_.toInt)
      .toList
      .sliding(3)
      .toList
      .map(_.sum)
      .foldLeft(0 -> Option.empty[Int]) { (accPrev, newX) =>
        accPrev._2 match
          case Some(oldX) if newX > oldX =>
            accPrev._1 + 1 -> newX.some

          case _ =>
            accPrev._1 -> newX.some
      }
      ._1
      .toString
