package com.htmlism.adventofcode
package year2021.day02

import scala.util.chaining._

import cats.data.NonEmptyList

object Problem2:
  def solve(xs: NonEmptyList[String]): String =
    xs
      .map { s =>
        val parts =
          s.split(" ")

        parts.head -> parts(1).toInt
      }
      .foldLeft(Position.zero) { (acc, e) =>
        e._1 match
          case "forward" =>
            acc.copy(horizontal = acc.horizontal + e._2, depth = acc.depth + acc.aim * e._2)

          case "down" =>
            acc.copy(aim = acc.aim + e._2)

          case "up" =>
            acc.copy(aim = acc.aim - e._2)
      }
      .pipe(x => x.horizontal * x.depth)
      .toString
