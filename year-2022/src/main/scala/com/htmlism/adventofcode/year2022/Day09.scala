package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import com.htmlism.adventofcode.core.*

object Day09:
  def apply(part: Part)(xs: List[String]): String =
    val snakeLength =
      part match
        case Part.One =>
          2
        case Part.Two =>
          10

    val snake =
      Snake(snakeLength)

    xs
      .map { s =>
        val Array(d, n) =
          s.split(" ")

        d -> n.toInt
      }
      .foldLeft(
        (Set(Coord(0, 0)), snake)
      ) { (acc, dn) =>
        val (d, n) =
          dn

        println
        println(dn)

        val (history, snake) =
          acc

//        println(d + ": " + history.toList.sortBy(_.toString))
//        println(d + ": " + headPosition + " " + tailPosition)

        val dir =
          dispatch(d)

        val (newHistory, newSnake) =
          (1 to n)
            .toList
            .foldLeft(history -> snake) { (acc, _) =>
              val newSnake =
                acc._2.nudge(dir)

//              newSnake.printSnake(Set.empty)
//              println

              val newHistory =
                acc._1 + newSnake.xs.last

              newHistory -> newSnake
            }

        (newHistory, newSnake)
      }
      .pipe { x =>
        x._2.printSnake(x._1)
        x
      }
      ._1
      .size
      .toString

  final case class Coord(x: Int, y: Int):
    def dist(that: Coord, by: Coord => Int): Int =
      by(this) - by(that)

  def assertNudge(head: Coord, tail: Coord, exp: Coord) =
    assert(
      nudgeT(head, tail) == exp,
      "\nHead: " + head.toString + "\nTail: " + tail.toString + "\nGot: " + nudgeT(head, tail).toString
    )

  assertNudge(Coord(0, 0), Coord(0, 0), Coord(0, 0))

  // close by? do nothing
  assertNudge(Coord(1, 0), Coord(0, 0), Coord(0, 0))
  assertNudge(Coord(0, 1), Coord(0, 0), Coord(0, 0))

  // one dimension? go to it
  assertNudge(Coord(2, 0), Coord(0, 0), Coord(1, 0))
  assertNudge(Coord(0, 2), Coord(0, 0), Coord(0, 1))

  // diagonal? tailwind
  assertNudge(Coord(2, 1), Coord(0, 0), Coord(1, 1))
  assertNudge(Coord(1, 2), Coord(0, 0), Coord(1, 1))

  // part two diagonal? corner
  assertNudge(Coord(2, 2), Coord(0, 0), Coord(1, 1))
  assertNudge(Coord(2, 2), Coord(0, 0), Coord(1, 1))

  def nudgeT(head: Coord, tail: Coord): Coord =
    val xDiff =
      head.dist(tail, _.x)

    val yDiff =
      head.dist(tail, _.y)

    Business("tail", tail).bmap { t =>
      if xDiff.abs > 1 then
        val newX =
          t.x + xDiff / 2

        val newY =
          if yDiff > 0 then t.y + 1
          else if yDiff < 0 then t.y - 1
          else t.y

        "x" -> Coord(newX, newY)
      else if yDiff.abs > 1 then
        val newY =
          t.y + yDiff / 2

        val newX =
          if xDiff > 0 then t.x + 1
          else if xDiff < 0 then t.x - 1
          else t.x

        "y" -> Coord(newX, newY)
      else "none" -> t
    }.x

  val dispatch: Map[String, Coord => Coord] =
    Map(
      "R" -> (c => c.copy(x = c.x + 1)),
      "L" -> (c => c.copy(x = c.x - 1)),
      "U" -> (c => c.copy(y = c.y + 1)),
      "D" -> (c => c.copy(y = c.y - 1))
    )

  case class Snake(minX: Int, maxX: Int, minY: Int, maxY: Int, xs: List[Coord]):
    def nudge(f: Coord => Coord): Snake =
      val newHead =
        f(xs.head)

      val withNewHead =
        xs
          .updated(0, f(xs.head))

      val newMinX = List(minX, newHead.x).min
      val newMaxX = List(maxX, newHead.x).max

      val newMinY = List(minY, newHead.y).min
      val newMaxY = List(maxY, newHead.y).max

      (0 until xs.size - 1)
        .foldLeft(withNewHead) { (acc, i) =>
          acc
            .updated(i + 1, nudgeT(acc(i), acc(i + 1)))
        }
        .pipe(xs => Snake(newMinX, newMaxX, newMinY, newMaxY, xs))

    def printSnake(hist: Set[Coord]): Unit =
      for y <- maxY to minY by -1
      do
        for x <- minX to maxX
        do
          xs
            .zipWithIndex
            .find(_._1 == Coord(x, y)) match
            case Some((_, i)) =>
              print(i)
            case None =>
              if hist(Coord(x, y)) then print("#")
              else print(".")
      println

  object Snake:
    def apply(knots: Int): Snake =
      Snake(0, 0, 0, 0, List.fill(knots)(Coord(0, 0)))
