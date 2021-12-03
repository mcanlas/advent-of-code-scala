package com.htmlism.adventofcode.year2021.day02

object Position {
  val zero: Position =
    Position(0, 0)
}

case class Position(horizontal: Int, depth: Int)
