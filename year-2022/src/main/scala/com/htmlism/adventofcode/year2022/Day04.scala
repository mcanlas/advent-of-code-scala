package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day04:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(
        _.split(",")
          .toList
          .map(ElfRange.fromStr)
      )
      .fproduct { xs =>
        val left =
          xs.head

        val right =
          xs(1)

        part match
          case Part.One =>
            left.contains(right) || right.contains(left)

          case Part.Two =>
            left.overlapedEither(right)

      }
      .map { x =>
        println(x); x._2
      }
      .filter(identity)
      .size
      .toString

  case class ElfRange(min: Int, max: Int):
    lazy val xs: Set[Int] =
      (min to max).toSet

    def contains(that: ElfRange): Boolean =
      (xs intersect that.xs) == xs

    def overlapedEither(that: ElfRange): Boolean =
      (xs intersect that.xs).nonEmpty

  object ElfRange:
    def fromStr(s: String): ElfRange =
      val Array(min, max) =
        s.split("-")

      ElfRange(min.toInt, max.toInt)
