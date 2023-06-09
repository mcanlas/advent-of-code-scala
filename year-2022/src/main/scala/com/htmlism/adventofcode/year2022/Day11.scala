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
          Nil -> (monkeys :+ Monkey.parse(proto :+ s))
        else
          (proto :+ s) -> monkeys
      }
      ._2
      .pipe { state =>
        val numRounds =
          part match
            case Part.One =>
              20
            case Part.Two =>
              10000

        (0 until numRounds)
          .foldLeft(state)((acc, _) => iterateRound(acc, worryDispatch(part)))
      }
      .pipe { xs =>
        printRound(xs)

        xs
          .map(_.inspectCount)
          .foreach(println)

        xs
          .map(_.inspectCount)
          .sorted
          .reverse
      }
      .pipe { mostActiveMonkeys =>
        mostActiveMonkeys(0) * mostActiveMonkeys(1)
      }
      .toString

  def worryDispatch(part: Part): Long => Long =
    part match
      case Part.One =>
        (_: Long) / 3L
      case Part.Two =>
        identity

  def printRound(xs: List[Monkey]): Unit =
    xs
      .zipWithIndex
      .map { case (m, i) =>
        s"Monkey $i: " + m.items.map(_.toString).mkString(", ")
      }
      .foreach(println)

  def iterateRound(xs: List[Monkey], worryAdjustment: Long => Long): List[Monkey] =
    xs
      .indices
      .foldLeft(xs) { (state, monkeyIndex) =>
        val curMonkey =
          state(monkeyIndex)

        val items =
          curMonkey
            .items
            .map(n => curMonkey.f(n))
            .map(worryAdjustment)

        println("Started with: " + curMonkey.items.mkString(", "))
        println("Became: " + items.mkString(", "))

        val monkeysAfterThrowing =
          items
            .foldLeft(state) { (stateThrown, thrownItem) =>
              val throwTarget =
                if (thrownItem % curMonkey.divisor == 0)
                  curMonkey.trueTarget
                else
                  curMonkey.falseTarget

              val targetMonkey =
                stateThrown(throwTarget)

              stateThrown
                .updated(throwTarget, targetMonkey.copy(items = targetMonkey.items :+ thrownItem))
            }

        println:
          monkeysAfterThrowing
            .updated(monkeyIndex, curMonkey.copy(items = Nil, inspectCount = curMonkey.inspectCount + items.size))

        monkeysAfterThrowing
          .updated(monkeyIndex, curMonkey.copy(items = Nil, inspectCount = curMonkey.inspectCount + items.size))
      }

  final case class Monkey(
      items: List[Long],
      f: Long => Long,
      divisor: Int,
      trueTarget: Int,
      falseTarget: Int,
      inspectCount: Int
  ):
    override def toString: String =
      "Monkey(" + items.mkString(", ") + ")"

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
          .map(_.toLong)
          .toList

      val Operation(operatorStr, operandStr) =
        xs(1): @unchecked

      val operation: Long => Long =
        (operatorStr, operandStr) match
          case ("*", "old") =>
            (n: Long) => n * n

          case ("*", n) =>
            (_ * n.toInt)

          case ("+", n) =>
            (_ + n.toInt)

          case _ =>
            sys.error(s"cannot parse $operatorStr $operandStr")

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
        falseTargetStr.toInt,
        0
      )

  assert(toPrimeFactors(2) == List(2))
  assert(toPrimeFactors(3) == List(3))
  assert(toPrimeFactors(10) == List(2, 5))
  assert(toPrimeFactors(11) == List(11))
  assert(toPrimeFactors(42) == List(2, 3, 7))

  def toPrimeFactors(n: Int): List[Int] =
    toPrimeFactorsRecur(n, 2, Nil)

  def toPrimeFactorsRecur(n: Int, divisor: Int, factors: List[Int]): List[Int] =
    if (n == 1) factors
    else
      assert(divisor <= n)

      if (n % divisor == 0)
        toPrimeFactorsRecur(n / divisor, divisor, factors :+ divisor)
      else
        toPrimeFactorsRecur(n, divisor + 1, factors)
