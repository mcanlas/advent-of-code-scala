package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.syntax.all.*

object Day05:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .pipe { xs =>
        val (instructionsFwd, movesStr) =
          twoStacks(xs)

        val crates =
          toLoadedStacks(instructionsFwd.reverse)

        val moves =
          movesStr
            .flatMap(toMoves(part))

        val crateAfterMoves =
          moves
            .foldLeft(crates)((acc, e) => acc.move(e))

        crateAfterMoves
      }
      .pipe(_.summarize)
      .toString

  def twoStacks(xs: List[String]) =
    val midpoint =
      xs
        .indexOf("")

    val first =
      xs.take(midpoint)

    val second =
      xs.slice(midpoint + 1, xs.size)

    first -> second

  val MovePattern =
    """move (\d+) from (\d+) to (\d+)""".r

  // these are indices by now
  case class Move(depth: Int, src: Int, dest: Int)

  private def toMoves(part: Part)(s: String) =
    val MovePattern(n, src, dest) = s: @unchecked

    part match
      case Part.One =>
        List.fill(n.toInt)(Move(1, src.toInt - 1, dest.toInt - 1))

      case Part.Two =>
        List(Move(n.toInt, src.toInt - 1, dest.toInt - 1))

  case class Crates(width: Int, stacks: List[List[Char]]):
    private lazy val indices =
      (0 until width)
        .map(_ * 4)
        .map(_ + 1)

    def extractCrateCmds(s: String): List[Char] =
      val asChars =
        s.toArray

      indices
        .map(i => asChars(i))
        .toList

    def push(i: Int, x: Char): Crates =
      if (x == ' ') this
      else
        val oldStack =
          stacks(i)

        copy(stacks = stacks.updated(i, oldStack :+ x))

    def move(mv: Move): Crates =
      val oldSource =
        stacks(mv.src)

      val oldDest =
        stacks(mv.dest)

      val elems =
        oldSource
          .slice(oldSource.size - mv.depth, oldSource.size)

      val newSource =
        oldSource
          .take(oldSource.size - mv.depth)

      val newDest =
        oldDest ::: elems

      copy(stacks =
        stacks
          .updated(mv.src, newSource)
          .updated(mv.dest, newDest)
      )

    def summarize: String =
      stacks
        .map(_.last.toString)
        .combineAll

  object Crates:
    def build(width: Int): Crates =
      Crates(width, List.fill(width)(Nil))

  private def toLoadedStacks(xs: List[String]) =
    val numStacks =
      xs.head.split("   ").size

    xs
      .tail
      .foldLeft(Crates.build(numStacks))((acc, e) => applyLayer(acc, e))

  private def applyLayer(acc: Crates, layerStr: String) =
    acc
      .extractCrateCmds(layerStr)
      .zipWithIndex
      .foldLeft(acc)((acc, e) => acc.push(e._2, e._1))
