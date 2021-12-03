package com.htmlism.adventofcode.year2021.day01

object StdinSolverTest extends App {
  var line: String = null
  val buff = new StringBuilder()
  do {
    line = Console.in.readLine()
    if (line != null) buff.append(line)
  } while (line != null)

  println(buff.toString())
}
