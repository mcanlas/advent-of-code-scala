package com.htmlism.adventofcode

import zio.*

object Runner extends ZIOAppDefault:
  val dispatch: Map[String, List[String] => String] =
    Map(
      "day1" -> (xs => xs.size.toString)
    )

  def run: ZIO[ZIOAppArgs, Exception, Unit] =
    for {
      args <- getArgs

      _ <- Console.printLine(args.toString)
    } yield ()
