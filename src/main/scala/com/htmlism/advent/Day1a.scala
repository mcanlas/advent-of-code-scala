package com.htmlism.advent
import scala.annotation.tailrec

import cats.data.NonEmptyList

object Day1a extends Solver:
  def solve(xs: NonEmptyList[String]): String =
    recurSolve(None, xs.map(_.toInt).toList)

  @tailrec
  private def recurSolve(acc: Option[Int], xs: List[Int]): String =
    acc match
      case Some(n) =>
        n.toString

      case None =>
        xs match
          case base :: tail =>
            val answerFromBase = foldTail(base, tail)

            answerFromBase match
              case Some(a) =>
                recurSolve(Some(a), Nil)

              case None =>
                recurSolve(None, tail)

          case Nil =>
            throw new IllegalArgumentException("input list had no solution")

  def foldTail(base: Int, xs: List[Int]): Option[Int] =
    xs
      .foldLeft(Option.empty[Int]) { (acc, e) =>
        acc match
          case Some(x) =>
            Some(x)

          case None =>
            if base + e == 2020 then Some(base * e)
            else None
      }
