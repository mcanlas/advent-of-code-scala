package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day10:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .foldLeft(Cpu(1) -> NonEmptyChain(Cpu(1))) { (acc, s) =>
        val (cpu, xs) =
          acc

        s match
          case "noop" =>
            cpu -> (xs :+ cpu)
          case Addx(n) =>
            val newCpu =
              cpu.copy(x = cpu.x + n.toInt)

            newCpu -> (xs :+ cpu :+ newCpu)

          case _ =>
            sys.error(s"cannot parse $s")
      }
      ._2
      .toNonEmptyVector
      .pipe { lookup =>
        part match
          case Part.One =>
            val ofInterest =
              List(20, 60, 100, 140, 180, 220)

            ofInterest
              .fproduct(n => lookup.getUnsafe(n - 1).x)
              .pipe { x =>
                println(x); x
              }
              .map(t => t._1 * t._2)
              .sum

          case Part.Two =>
            (0 until 6)
              .map { y =>
                (1 to 40)
                  .map { x =>
                    val stateIndex = x + (y * 40)

                    val drawPosition =
                      lookup.getUnsafe(stateIndex - 1).x

                    if (drawPosition - 1 <= x - 1 && x - 1 <= drawPosition + 1)
                      "#"
                    else
                      "."
                  }
                  .mkString("")
              }
              .mkString("\n")
      }
      .toString

  case class Cpu(x: Int)

  val Addx =
    """addx (-?\d+)""".r
