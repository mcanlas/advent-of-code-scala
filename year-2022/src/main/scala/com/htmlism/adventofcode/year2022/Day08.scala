package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day08:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(_.toList.map(_.toString.toInt))
      .pipe(TreeGrid(_))
      .pipe { grid =>
        grid
          .trees
          .map { coord =>
            val height =
              grid(coord).getOrElse(sys.error("height available only for trees that exist"))

            height -> grid.toHeights(coord)
          }
          .pipe { xs =>
            part match
              case Part.One =>
                xs
                  .map((grid.treeIsVisible _).tupled)
                  .count(identity)

              case Part.Two =>
                xs
                  .fproduct((grid.scenicScore _).tupled)
                  .foreach(println)

                xs
                  .map((grid.scenicScore _).tupled)
                  .map(_.product)
                  .max
          }
      }
      .toString

  final case class TreeGrid(size: Int, xs: List[List[Int]]):
    import TreeGrid._

    def trees: List[Coord] =
      (for {
        y <- 0 until size
        x <- 0 until size
      } yield Coord(x, y)).toList

    def apply(xy: Coord): Option[Int] =
      for {
        row    <- xs.get(xy.y)
        height <- row.get(xy.x)
      } yield height

    def toHeights(tree: Coord): List[List[Int]] =
      TreeGrid
        .visibilityCriteria
        .map(f => accHeights(f, f(tree), Nil))

    def treeIsVisible(height: Int, dirs: List[List[Int]]): Boolean =
      dirs
        .map(xs => xs.forall(_ < height))
        .reduce(_ || _)

    def scenicScore(height: Int, dirs: List[List[Int]]) =
      dirs
        .map(xs => viewableHeights(0, xs).size)

    // TODO state monad?
    private def viewableHeights(tree: Int, neighbors: List[Int]) =
      neighbors
        .foldLeft(tree -> List.empty[Int]) { (acc, e) =>
          val (minHeight, keeps) = acc

          if (e >= minHeight)
            e         -> (keeps :+ e)
          else
            minHeight -> keeps
        }
        ._2

    @tailrec
    def accHeights(
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
