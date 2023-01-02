package com.htmlism.adventofcode.year2022

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day02:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map { str =>
        val xs =
          str.split(" ")

        val them =
          parse(xs(0))

        val me =
          selectStrategy(them, part)(xs(1))

        val fightScore =
          rpsCycle
            .compare(me, them) match
            case n if n > 0 =>
              6
            case n if n < 0 =>
              0
            case _ =>
              3

        println(s"$me $them $fightScore")

        fightScore + shapeScore(me)
      }
      .sumAll
      .toString

  def selectStrategy(them: Rps, part: Part): String => Rps =
    part match
      case Part.One =>
        Map(
          "X" -> Rps.Rock,
          "Y" -> Rps.Paper,
          "Z" -> Rps.Scissors
        )

      case Part.Two =>
        Map(
          "X" -> rpsCycle.getLoser(them),
          "Y" -> them,
          "Z" -> rpsCycle.getWinner(them)
        )

  enum Rps:
    case Rock
    case Paper
    case Scissors

  val rpsCycle =
    Cycle(
      List(
        Rps.Rock,
        Rps.Paper,
        Rps.Scissors
      )
    )

  val parse =
    Map(
      "A" -> Rps.Rock,
      "B" -> Rps.Paper,
      "C" -> Rps.Scissors
    )

  def shapeScore(x: Rps) =
    x match
      case Rps.Rock     => 1
      case Rps.Paper    => 2
      case Rps.Scissors => 3

  case class Cycle[A](xs: List[A]):
    private val lookup =
      xs
        .zipWithIndex
        .toMap

    def compare(x: A, y: A): Int =
      val xn =
        lookup(x)

      val yn =
        lookup(y)

      if (xn == (xs.size - 1) && yn == 0)
        -1
      else if (xn == 0 && yn == (xs.size - 1))
        1
      else
        Order[Int].compare(xn, yn)

    def getWinner(x: A): A =
      val xn =
        lookup(x)

      if (xn == (xs.size - 1))
        xs.head
      else
        xs(xn + 1)

    def getLoser(x: A): A =
      val xn =
        lookup(x)

      if (xn == 0)
        xs.last
      else
        xs(xn - 1)
