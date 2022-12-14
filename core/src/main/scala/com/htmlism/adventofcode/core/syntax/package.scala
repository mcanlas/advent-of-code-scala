package com.htmlism.adventofcode.core

import cats._
import cats.syntax.all._

package object syntax:
  implicit class BusinessFoldableOps[F[_]: Foldable, A](fa: F[A]):
    def bfoldLeft[B](zero: B)(f: (B, A) => (String, B)) =
      fa.foldLeft(zero.pure[Business])((acc, e) =>
        val (s, b) = f(acc.x, e)

        acc *> Business(s, b)
      )
