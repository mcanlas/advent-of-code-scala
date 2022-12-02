package com.htmlism.advent

import cats.data.NonEmptyList

trait Solver:
  def solve(xs: NonEmptyList[String]): String
