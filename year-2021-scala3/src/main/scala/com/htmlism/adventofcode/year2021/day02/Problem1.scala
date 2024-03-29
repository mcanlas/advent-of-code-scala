package com.htmlism.adventofcode
package year2021.day02

import scala.util.chaining.*

import cats.data.NonEmptyList

object Problem1:
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
            acc.copy(horizontal = acc.horizontal + e._2)

          case "down" =>
            acc.copy(depth = acc.depth + e._2)

          case "up" =>
            acc.copy(depth = acc.depth - e._2)
      }
      .pipe(x => x.horizontal * x.depth)
      .toString
