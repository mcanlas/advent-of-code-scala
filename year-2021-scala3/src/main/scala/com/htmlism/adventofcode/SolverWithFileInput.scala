package com.htmlism.adventofcode

import cats.data._
import cats.effect._

trait SolverWithFileInput extends IOApp {
  private lazy val reader =
    new BetterFilesReader[IO]

  def solve(xs: NonEmptyList[String]): String

  def run(args: List[String]): IO[ExitCode] =
    for {
      xs <- reader.lines(args.head)

      _ <- IO {
             println(solve(xs))
           }
    } yield ExitCode.Success
}
