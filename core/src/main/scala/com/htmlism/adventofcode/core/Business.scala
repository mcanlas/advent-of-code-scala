package com.htmlism.adventofcode.core

import cats._
import cats.data._
import cats.syntax.all._

import com.htmlism.adventofcode.core.syntax._

final case class Business[A](log: Chain[String], depth: Int, x: A):
  def bmap[B](fs: A => String, fb: A => B): Business[B] =
    val newLine =
      ("  " * depth) + fs(x)

    Business(
      log :+ newLine,
      depth + 1,
      fb(x)
    )

  def printAndGet(): A =
    log
      .iterator
      .foreach(println)

    x

  /**
    * `Business` can merge logs with `Applicative` but builds hierarchical structure using `flatMap` syntax. This
    * hierarchy makes it not a lawful monad (e.g. (A link B) link C != A link (B link C))
    */
  def flatMap[B](f: A => Business[B]): Business[B] =
    val fb = f(x)

    Business(log ++ fb.log.map("  " + _), depth + 1, fb.x)

object Business:
  def apply[A](s: String, x: A): Business[A] =
    Business(Chain(s), depth = 1, x)

  given Applicative[Business] with
    def pure[A](x: A): Business[A] =
      Business(Chain.empty, depth = 0, x)

    def ap[A, B](ff: Business[A => B])(fa: Business[A]): Business[B] =
      Business(ff.log ++ fa.log, depth = 0, ff.x(fa.x))

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
    .printAndGet()

  List(1, 2, 3)
    .traverse { n =>
      Business("start", n)
        .bmap(_ => "plus one", _ + 1)
        .bmap(_ => "plus two", _ + 2)
    }
    .bmap(_ => "summarize", _.sum)
    .printAndGet()

  List(4, 5, 6)
    .bfoldLeft(0) { (acc, e) =>
      (s"$acc + $e", acc + e)
    }
    .printAndGet()

  (for {
    x <- Business("abc", 123)

    y <- Business("plus one", x + 1)
  } yield y)
    .printAndGet()
