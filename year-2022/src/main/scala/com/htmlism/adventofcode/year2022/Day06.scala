package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day06:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(_.toList)
      .map(processSignal(messageWidth(part)))
      .mkString("\n")
      .toString

  private def processSignal(width: Int)(xs: List[Char]) =
    xs
      .sliding(width)
      .toList
      .indexWhere(fragmentIsPositive(width)) + width

  private def fragmentIsPositive(width: Int)(xs: List[Char]) =
    assert(xs.length == width)

    xs.toSet.size == width

  private def messageWidth(part: Part) =
    part match
      case Part.One =>
        4

      case Part.Two =>
        14
