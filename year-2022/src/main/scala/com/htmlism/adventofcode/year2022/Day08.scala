package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.adventofcode.core._

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

            (coord, height, grid.toHeights(coord))
          }
          .pipe { xs =>
            part match
              case Part.One =>
                xs
                  .map { case (_, height, heights) => grid.treeIsVisible(height, heights) }
                  .count(identity)

              case Part.Two =>
                xs
                  .traverse { case (coord, height, dirs) =>
                    Business(s"$coord height $height", height)
                      .flatMap { n =>
                        dirs
                          .traverse { hs =>
                            val score =
                              grid.scenicScore(n, hs)

                            Business(s"${hs.toString} => $score", score)
                          }
                      }
                      .bmap(xs => s"scenic score ${xs.product}", _.product)
                  }
                  .bmap(xs => "max: " + xs.max, _.max)
                  .printAndGet()
          }
      }
      .toString

  final case class TreeGrid(size: Int, xs: List[List[Int]]):
    import TreeGrid._

    def trees: List[Coord] =
      (for {
        y <- 0 until size
        x <- 0 until size
      } yield Coord(y, x)).toList

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

    // `zipWithIndex` strategy taken from someone's scala solution on reddit =(
    // solution was to stop at the first tree that is gte the center tree
    // reading comprehension is hard

    // we can see past the middle 3
    assert(scenicScore(4, List(2, 3, 2)) == 3)

    def scenicScore(centerHeight: Int, neighbors: List[Int]): Int =
      neighbors
<<<<<<< HEAD
        .foldLeft(0 -> List.empty[Int]) { (acc, e) =>
          val (minHeight, keeps) = acc

<<<<<<< HEAD
          if (e >= minHeight)
            e -> (keeps :+ e)
=======
          if (e >= minHeight && e <= centerHeight)
            e            -> (keeps :+ e)
>>>>>>> 53a370f (not correct)
          else
            Int.MaxValue -> keeps
        }
        ._2
=======
        .zipWithIndex
        .find(_._1 >= centerHeight)
        .map(_._2 + 1)
        .getOrElse(neighbors.size)
>>>>>>> 8cf7fea (solved)

    @tailrec
    def accHeights(
        f: Coord => Coord,
        toQuery: Coord,
        acc: List[Int]
    ): List[Int] =
      apply(toQuery) match
        case Some(neighborHeight) =>
          accHeights(f, f(toQuery), acc :+ neighborHeight)
        case None =>
          acc

  object TreeGrid:
    def apply(xs: List[List[Int]]): TreeGrid =
      TreeGrid(xs.size, xs)

    case class Coord(y: Int, x: Int)

    val visibilityCriteria =
      List[Coord => Coord](
        a => a.copy(y = a.y - 1), // up
        a => a.copy(x = a.x - 1), // left
        a => a.copy(x = a.x + 1), // right
        a => a.copy(y = a.y + 1)  // down
      )
