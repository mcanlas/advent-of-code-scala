package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day08:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(_.toList.map(_.toInt))
      .pipe(TreeGrid(_))
      .pipe { grid =>
        grid
          .trees
          .map(grid.treeIsVisible)
          .count(identity)
      }
      .toString

  case class TreeGrid(size: Int, xs: List[List[Int]]):
    import TreeGrid._

    def trees: List[Coord] =
      (for {
        x <- 0 until size
        y <- 0 until size
      } yield Coord(x, y)).toList

    def apply(xy: Coord): Option[Int] =
      for {
        row <- xs.get(xy.y)
        n   <- row.get(xy.x)
      } yield n

    def treeIsVisible(tree: Coord): Boolean =
      val height =
        apply(tree).getOrElse(sys.error("visibility check only available for trees that exist"))

      TreeGrid
        .visibilityCriteria
        .map(f => accHeights(f, f(tree), Nil))
        .map(xs => xs.forall(_ < height))
        .reduce(_ || _)

    @tailrec
    final def accHeights(
        f: Coord => Coord,
        toQuery: Coord,
        acc: List[Int]
    ): List[Int] =
      apply(toQuery) match
        case Some(neighborHeight) =>
          accHeights(f, f(toQuery), acc :+ neighborHeight)
        case None                 =>
          acc

  object TreeGrid:
    def apply(xs: List[List[Int]]): TreeGrid =
      TreeGrid(xs.size, xs)

    case class Coord(x: Int, y: Int)

    val visibilityCriteria =
      List[Coord => Coord](
        a => a.copy(y = a.y - 1), // up
        a => a.copy(x = a.x - 1), // left
        a => a.copy(x = a.x + 1), // right
        a => a.copy(y = a.y + 1)  // down
      )
