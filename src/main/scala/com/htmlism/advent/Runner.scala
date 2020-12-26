package com.htmlism.advent

import cats.effect._
import cats.syntax.all._

object Runner extends Runner[IO] with IOApp {
  val dispatch: Map[String, Solver] =
    Map(
      "1a" -> Day1a,
    )
}

class Runner[F[_]](implicit F: Async[F]) {
  def run(args: List[String]): F[ExitCode] =
    for {
      adventArgs <- AdventArgs.extract(args)

      xs <- new BetterFilesReader[F].lines(adventArgs.fileName)

      solver = Runner.dispatch(adventArgs.solverKey)

      _ <- F.delay(println(solver.solve(xs)))
    } yield ExitCode.Success
}

case class AdventArgs(solverKey: String, fileName: String)

object AdventArgs {
  def extract[F[_]](args: List[String])(implicit F: Async[F]): F[AdventArgs] =
    args match {
      case solverKey :: fileName :: Nil =>
        F.delay {
          AdventArgs(solverKey, fileName)
        }

      case _ =>
        F.raiseError(new IllegalArgumentException("need <solver key> and <file name>"))
    }
}
