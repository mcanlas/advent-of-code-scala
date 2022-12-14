package com.htmlism.adventofcode.core

import cats._
import cats.data._
import cats.syntax.all._

import com.htmlism.adventofcode.core.syntax._

final case class Business[A](log: Chain[String], stack: Chain[String], x: A):
  def bmap[B](fs: A => String, fb: A => B): Business[B] =
    copy(stack = stack :+ fs(x), x = fb(x))

  def bothLog: Chain[String] =
    stack
      .foldLeft(0 -> log) { (acc, s) =>
        val (depth, logs) = acc

        (depth + 1) -> (logs :+ ("  " * depth) + s)
      }
      ._2

object Business:
  def apply[A](s: String, x: A): Business[A] =
    Business(Chain.empty, Chain(s), x)

  given Applicative[Business] with
    def pure[A](x: A): Business[A] =
      Business(Chain.empty, Chain.empty, x)

    def ap[A, B](ff: Business[A => B])(fa: Business[A]): Business[B] =
      Business(ff.bothLog ++ fa.bothLog, Chain.empty, ff.x(fa.x))

object Demo extends App:
  val a =
    Applicative[Business]
      .pure(123)
      .bmap(_ => "plus", _ + 1)

  val b =
    Applicative[Business]
      .pure(456)
      .bmap(_ => "plus", _ + 1)

  (a, b)
    .tupled
    .bmap(_ => "hello", identity)
    .bothLog
    .toList
    .foreach(println)

  List(1, 2, 3)
    .traverse { n =>
      Business("start", n)
        .bmap(_ => "plus one", _ + 1)
        .bmap(_ => "plus two", _ + 2)
    }
    .bmap(_ => "summarize", _.sum)
    .bothLog
    .toList
    .foreach(println)

  List(4, 5, 6)
    .bfoldLeft(0) { (acc, e) =>
      (s"$acc + $e", acc + e)
    }
    .bothLog
    .toList
    .foreach(println)
