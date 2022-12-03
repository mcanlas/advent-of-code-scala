package com.htmlism.adventofcode.year2022

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day02:
  def apply(xs: List[String]): String =
    xs
      .map { str =>
        val Array(them, me) =
          str.split(" ").map(parse)

        fight(them, me) + shapeScore(me)
      }
      .sumAll
      .toString

  enum Rps:
    case Rock
    case Paper
    case Scissors

  val parse =
    Map(
      "A" -> Rps.Rock,
      "B" -> Rps.Paper,
      "C" -> Rps.Scissors,
      "X" -> Rps.Rock,
      "Y" -> Rps.Paper,
      "Z" -> Rps.Scissors
    )

  def shapeScore(x: Rps) =
    x match
      case Rps.Rock     => 1
      case Rps.Paper    => 2
      case Rps.Scissors => 3

  def fight(them: Rps, me: Rps): Int =
    (them, me) match
      case (Rps.Rock, Rps.Paper)    => 6
      case (Rps.Rock, Rps.Scissors) => 0

      case (Rps.Paper, Rps.Rock)     => 0
      case (Rps.Paper, Rps.Scissors) => 6

      case (Rps.Scissors, Rps.Rock)  => 6
      case (Rps.Scissors, Rps.Paper) => 0

      case _ => 3
