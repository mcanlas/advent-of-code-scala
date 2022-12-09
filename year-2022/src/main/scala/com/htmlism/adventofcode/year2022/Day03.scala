package com.htmlism.adventofcode.year2022

import scala.util.chaining._

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day03:
  def apply(xs: List[String]): String =
    xs
      .map(_.toList)
      .map { chars =>
        chars
          .pipe(splitEvenly)
          .map(_.toSet)
          .reduce(_ intersect _)
      }
      .map(_.toList.head)
      .map(toPriority)
      .sumAll
      .toString

  private def splitEvenly[A](xs: List[A]) =
    val take =
      xs.size / 2

    val first =
      xs.take(take)

    val second =
      xs.slice(take, xs.size)

    assert(first.size == second.size)

    List(first, second)

  private val toPriority =
    (('a' to 'z') ++ ('A' to 'Z'))
      .zipWithIndex
      .map(_.map(_ + 1))
      .toMap
