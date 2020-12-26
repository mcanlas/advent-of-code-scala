package com.htmlism.advent

import better.files._
import cats.data._
import cats.effect._
import cats.syntax.all._

trait FileReaderAlg[F[_]] {
  def lines(path: String): F[NonEmptyList[String]]
}

class BetterFilesReader[F[_]](implicit F: Sync[F]) extends FileReaderAlg[F] {
  def lines(path: String): F[NonEmptyList[String]] =
    F.delay {
      File(path).lines
    }.map(_.toList)
      .map(NonEmptyList.fromList[String])
      .flatMap {
        case Some(xs) =>
          xs.pure[F]

        case None =>
          F.raiseError(new RuntimeException(s"file $path was empty"))
      }
}
