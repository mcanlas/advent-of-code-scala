package com.htmlism.adventofcode

import zio.*

import com.htmlism.rufio.withzio.*

object Runner extends ZIOAppDefault:
  val dispatch: Map[String, List[String] => String] =
    Map(
      "day1" -> (xs => xs.size.toString)
    )

  def run: ZIO[ZIOAppArgs, Throwable, Unit] =
    for {
      args <- getArgs

      xs <- File(args(1)).getLines

      _ <- Console.printLine(xs)
    } yield ()
