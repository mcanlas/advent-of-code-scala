package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day07:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(TerminalOutput.parse)
      .mkString("\n")
      .toString

  object Pat:
    val ChangeDirectory =
      """\$ cd (\S+)""".r

    val List =
      """\$ ls""".r

    val Dir =
      """dir (\S+)""".r

    val File =
      """(\d+) (\S+)""".r

  object TerminalOutput:
    def parse(s: String): TerminalOutput =
      s match
        case Pat.ChangeDirectory(dir) =>
          dir match
            case "/" =>
              ToRoot

            case ".." =>
              GoUpDirectory

            case d =>
              AddDirectory(d)

        case Pat.List() =>
          List

        case Pat.Dir(d) =>
          AddDirectory(d)

        case Pat.File(s, f) =>
          AddFile(s.toInt, f)

  enum TerminalOutput:
    case ToRoot
    case List
    case AddDirectory(s: String)
    case AddFile(size: Int, name: String)
    case ChangeDirectory(s: String)
    case GoUpDirectory
