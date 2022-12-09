package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
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
            .flatMap(toMoves)

        val crateAfterMoves =
          moves
            .foldLeft(crates)((acc, e) => acc.move(e.src, e.dest))

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
  case class Move(src: Int, dest: Int)

  private def toMoves(s: String) =
    val MovePattern(n, src, dest) = s: @unchecked

    List.fill(n.toInt)(Move(src.toInt - 1, dest.toInt - 1))

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

    def move(src: Int, dest: Int): Crates =
      val oldSource =
        stacks(src)

      val oldDest =
        stacks(dest)

      val elem =
        oldSource.last

      val newSource =
        oldSource
          .take(oldSource.size - 1)

      val newDest =
        oldDest :+ elem

      copy(stacks =
        stacks
          .updated(src, newSource)
          .updated(dest, newDest)
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
