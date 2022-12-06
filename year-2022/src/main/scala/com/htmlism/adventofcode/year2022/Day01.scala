package com.htmlism.adventofcode.year2022

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day01:
  def apply(take: Int)(xs: List[String]): String =
    xs
      .traverse(accCmd)
      .runS(NonEmptyList.one(Nil))
      .value
      .map(_.sumAll)
      .sorted(Order.reverse(Order[Int]))
      .take(take)
      .toString

  private def accCmd(cmd: String) =
    State { (xs: NonEmptyList[List[Int]]) =>
      if (cmd.isEmpty)
        (Nil :: xs)                                 -> cmd
      else
        NonEmptyList(cmd.toInt :: xs.head, xs.tail) -> cmd
    }
