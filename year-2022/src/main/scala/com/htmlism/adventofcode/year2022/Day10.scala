package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.adventofcode.core._

object Day10:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .foldLeft(Cpu(1) -> NonEmptyChain(Cpu(1))) { (acc, s) =>
        val (cpu, xs) =
          acc

        s match
          case "noop"  =>
            cpu -> (xs :+ cpu)
          case Addx(n) =>
            val newCpu =
              cpu.copy(x = cpu.x + n.toInt)

            newCpu -> (xs :+ cpu :+ newCpu)

          case _ =>
            sys.error(s"cannot parse $s")
      }
      ._2
      .pipe { signalHistory =>
        val ofInterest =
          List(20, 60, 100, 140, 180, 220)

        val lookup =
          signalHistory.toNonEmptyVector

        println(signalHistory.size)

        ofInterest
          .fproduct(n => lookup.getUnsafe(n - 1).x)
          .pipe { x =>
            println(x); x
          }
          .map(t => t._1 * t._2)
          .sum
      }
      .toString

  case class Cpu(x: Int)

  val Addx =
    """addx (-?\d+)""".r
