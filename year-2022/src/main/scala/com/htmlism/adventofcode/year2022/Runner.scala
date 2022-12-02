package com.htmlism.adventofcode.year2022

import com.htmlism.rufio.withzio.*
import zio.*

object Runner extends ZIOAppDefault:
  val dispatch: Map[String, List[String] => String] =
    Map(
      "day1" -> Day01.apply
    )

  def run: ZIO[ZIOAppArgs, Throwable, Unit] =
    for {
      args <- getArgs

      xs <- File(args(1)).getLines

      s <- Console.printLine(dispatch(args(0))(xs))
    } yield s
