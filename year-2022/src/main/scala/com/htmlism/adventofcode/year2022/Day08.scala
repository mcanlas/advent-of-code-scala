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
    def trees: List[TreeGrid.Coord] =
      (for {
        x <- 0 until size
        y <- 0 until size
      } yield TreeGrid.Coord(x, y)).toList

    def apply(xy: TreeGrid.Coord): Option[Int] =
      for {
        row <- xs.get(xy.y)
        n   <- row.get(xy.x)
      } yield n

    def treeIsVisible(tree: TreeGrid.Coord): Boolean =
      val height =
        apply(tree).getOrElse(sys.error("visibility check only available for trees that exist"))

      TreeGrid
        .visibilityCriteria
        .map(f => treeIsVisibleFromOneDir(height, f, f(tree)))
        .reduce(_ || _)

    @tailrec
    final def treeIsVisibleFromOneDir(
        treeHeight: Int,
        f: TreeGrid.Coord => TreeGrid.Coord,
        toQuery: TreeGrid.Coord
    ): Boolean =
      apply(toQuery) match
        case Some(neighborHeight) =>
          if (neighborHeight < treeHeight)
            treeIsVisibleFromOneDir(treeHeight, f, f(toQuery))
          else
            false
        case None                 =>
          true

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
