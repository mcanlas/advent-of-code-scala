package com.htmlism.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.adventofcode.core._

object Day11:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .foldLeft(List.empty[String] -> List.empty[Monkey]) { (acc, s) =>
        val (proto, monkeys) =
          acc

        if (s.startsWith("Monkey") || s == "")
          proto -> monkeys
        else if (proto.size == 4)
          Nil          -> (monkeys :+ Monkey.parse(proto :+ s))
        else
          (proto :+ s) -> monkeys
      }
      .toString

  final case class Monkey(items: List[Int], f: Int => Int, divisor: Int, trueTarget: Int, falseTarget: Int)

  object Monkey:
    val StartingItems =
      """Starting items: (.+)""".r.unanchored

    val Operation =
      """Operation: new = old (.) (.+)""".r.unanchored

    val DivisibleBy =
      """Test: divisible by (.+)""".r.unanchored

    val Target =
      """monkey (.+)""".r.unanchored

    def parse(xs: List[String]): Monkey =
      assert(xs.size == 5)

      val StartingItems(itemsStr) =
        xs(0): @unchecked

      val items =
        itemsStr
          .split(", ")
          .map(_.toInt)
          .toList

      val Operation(operatorStr, operandStr) =
        xs(1): @unchecked

      val operationHole =
        operatorStr match
          case "*" =>
            (y: Int) => (x: Int) => x * y

          case "+" =>
            (y: Int) => (x: Int) => x + y

          case _ =>
            sys.error(s"cannot parse $operatorStr")

      val operation =
        operandStr match
          case "old" =>
            (n: Int) => operationHole(n)(n)

          case n =>
            operationHole(n.toInt)

      val DivisibleBy(divisorStr) =
        xs(2): @unchecked

      val Target(trueTargetStr) =
        xs(3): @unchecked

      val Target(falseTargetStr) =
        xs(4): @unchecked

      Monkey(
        items,
        operation,
        divisorStr.toInt,
        trueTargetStr.toInt,
        falseTargetStr.toInt
      )
