package com.htmlism.adventofcode.year2022

import zio.*

import com.htmlism.rufio.withzio.*

object Runner extends ZIOAppDefault:
  val dispatch: Map[String, List[String] => String] =
    Map(
      "day10"       -> Day10(Part.One).apply,
      "day10.part2" -> Day10(Part.Two).apply,
      "day9"        -> Day09(Part.One).apply,
      "day9.part2"  -> Day09(Part.Two).apply,
      "day8"        -> Day08(Part.One).apply,
      "day8.part2"  -> Day08(Part.Two).apply,
      "day7"        -> Day07(Part.One).apply,
      "day7.part2"  -> Day07(Part.Two).apply,
      "day6"        -> Day06(Part.One).apply,
      "day6.part2"  -> Day06(Part.Two).apply,
      "day5"        -> Day05(Part.One).apply,
      "day5.part2"  -> Day05(Part.Two).apply,
      "day4"        -> Day04(Part.One).apply,
      "day4.part2"  -> Day04(Part.Two).apply,
      "day3"        -> Day03(Part.One).apply,
      "day3.part2"  -> Day03(Part.Two).apply,
      "day1"        -> Day01(take = 1).apply,
      "day1.part2"  -> Day01(take = 3).apply,
      "day2"        -> Day02(Part.One).apply,
      "day2.part2"  -> Day02(Part.Two).apply
    )

  def run: ZIO[ZIOAppArgs, Throwable, Unit] =
    for {
      args <- getArgs

      xs <- File(args(1)).getLines

      s <- Console.printLine(dispatch(args(0))(xs))
    } yield s
