package com.htmlism.adventofcode.year2022

import zio.*

import com.htmlism.rufio.withzio.*

object Runner extends ZIOAppDefault:
  val dispatch: Map[String, List[String] => String] =
    Map(
      "day3"       -> Day03.apply,
      "day1"       -> Day01(take = 1).apply,
      "day1.part2" -> Day01(take = 3).apply,
      "day2"       -> Day02(Part.One).apply,
      "day2.part2" -> Day02(Part.Two).apply
    )

  def run: ZIO[ZIOAppArgs, Throwable, Unit] =
    for {
      args <- getArgs

      xs <- File(args(1)).getLines

      s <- Console.printLine(dispatch(args(0))(xs))
    } yield s
