package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day06:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(_.toList)
      .map(processSignal)
      .mkString("\n")
      .toString

  private def processSignal(xs: List[Char]) =
    xs
      .sliding(4)
      .toList
      .indexWhere(fragmentIsPositive) + 4

  private def fragmentIsPositive(xs: List[Char]) =
    assert(xs.length == 4)

    println(xs)

    xs.toSet.size == 4
