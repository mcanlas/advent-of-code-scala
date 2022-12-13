package com.htmlism.adventofcode.year2022

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day01:
  def apply(take: Int)(xs: List[String]): String =
    xs
      .foldLeft(NonEmptyList.one(List.empty[Int]))((xs, cmd) =>
        if (cmd.isEmpty)
          (Nil :: xs)
        else
          NonEmptyList(cmd.toInt :: xs.head, xs.tail)
      )
      .map(_.sumAll)
      .sorted(Order.reverse(Order[Int]))
      .take(take)
      .sumAll
      .toString
