package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.syntax.all.*

object Day03:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .pipe(toRucksacks(part))
      .map { sacks =>
        sacks
          .map(_.toSet)
          .reduce(_ intersect _)
      }
      .map(_.toList.head)
      .map(toPriority)
      .sumAll
      .toString

  // the return type here is hilarious
  private def toRucksacks(part: Part)(xs: List[String]) =
    part match
      case Part.One =>
        xs
          .map(_.toList)
          .map { chars =>
            chars
              .pipe(splitEvenly)
          }

      case Part.Two =>
        xs
          .map(_.toList)
          .grouped(3) // this method is amazing, thank you standard library
          .toList

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
