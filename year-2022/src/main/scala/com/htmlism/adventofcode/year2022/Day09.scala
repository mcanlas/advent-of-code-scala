package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.adventofcode.core._

object Day09:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .iterator
      .flatMap { s =>
        val Array(d, n) =
          s.split(" ")

        List
          .fill(n.toInt)(d)
      }
      .foldLeft(
        (Set(Coord(0, 0)), Coord(0, 0), Coord(0, 0))
      ) { (acc, d) =>
        val (history, headPosition, tailPosition) =
          acc

//        println(d + ": " + history.toList.sortBy(_.toString))
//        println(d + ": " + headPosition + " " + tailPosition)

        val dir =
          dispatch(d)

        val newHeadPosition =
          dir(headPosition)

        assert(newHeadPosition.dist(tailPosition, _.x).abs <= 2, newHeadPosition.toString + " " + tailPosition.toString)
        assert(newHeadPosition.dist(tailPosition, _.y).abs <= 2, newHeadPosition.toString + " " + tailPosition.toString)

        val newTailPosition =
          nudgeT(newHeadPosition, tailPosition).x

        println(s"$d $newHeadPosition $newTailPosition")

        val newHistory =
          history + newTailPosition

//        newHistory
//          .pipe { xs =>
//            for {
//              y <- xs.toList.map(_.y).max to 0 by -1
//            } yield
//              for {
//                x <- 0 to xs.toList.map(_.x).max
//              } yield
//                if (xs.contains(Coord(x, y)))
//                  print("x")
//                else
//                  print(".")
//              println
//
//            xs
//          }

        (newHistory, newHeadPosition, newTailPosition)
      }
      ._1
      .size
      .toString

  final case class Coord(x: Int, y: Int):
    def dist(that: Coord, by: Coord => Int): Int =
      by(this) - by(that)

  assert(nudgeT(Coord(0, 0), Coord(0, 0)).x == Coord(0, 0))

  // close by? do nothing
  assert(nudgeT(Coord(1, 0), Coord(0, 0)).x == Coord(0, 0))
  assert(nudgeT(Coord(0, 1), Coord(0, 0)).x == Coord(0, 0))

  // one dimension? go to it
  assert(nudgeT(Coord(2, 0), Coord(0, 0)).x == Coord(1, 0))
  assert(nudgeT(Coord(0, 2), Coord(0, 0)).x == Coord(0, 1))

  // diagonal? tailwind
  assert(nudgeT(Coord(2, 1), Coord(0, 0)).x == Coord(1, 1))
  assert(nudgeT(Coord(1, 2), Coord(0, 0)).x == Coord(1, 1))

  def nudgeT(head: Coord, tail: Coord): Business[Coord] =
    tail
      .pure[Business]
      .bmap(t =>
        head.dist(t, _.x) match {
          case n if n > 1 =>
            "move right" -> t.copy(x = t.x + 1, y = head.y)

          case n if n < -1 =>
            "move left" -> t.copy(x = t.x - 1, y = head.y)

          case _ =>
            "no x" -> t
        }
      )
      .bmap(t =>
        head.dist(t, _.y) match {
          case n if n > 1 =>
            "move up" -> t.copy(y = t.y + 1, x = head.x)

          case n if n < -1 =>
            "move down" -> t.copy(y = t.y - 1, x = head.x)

          case _ =>
            "no y" -> t
        }
      )

  val dispatch: Map[String, Coord => Coord] =
    Map(
      "R" -> (c => c.copy(x = c.x + 1)),
      "L" -> (c => c.copy(x = c.x - 1)),
      "U" -> (c => c.copy(y = c.y + 1)),
      "D" -> (c => c.copy(y = c.y - 1))
    )
