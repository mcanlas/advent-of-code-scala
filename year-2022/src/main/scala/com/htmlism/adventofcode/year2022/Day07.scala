package com.htmlism.adventofcode.year2022

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

object Day07:
  def apply(part: Part)(xs: List[String]): String =
    xs
      .map(TerminalOutput.parse)
      .traverse(toStatelessCmd)
      .runA(CurrentWorkingDirectory.empty)
      .value

      // drop terminal output that did not produce absolute file paths
      .flatMap(_.toList)
      .pipe { xs =>
        val uniqueDirs =
          xs
            .map(_.slug)
            .distinct

        val dirsWithTotalSize =
          uniqueDirs
            .fproduct { slug =>
              xs
                .filter(_.slug.startsWith(slug))
                .map(_.x.size)
                .sum
            }

        dirsWithTotalSize
      }
      .pipe(solve(part))
      .toString

  private def solve(part: Part)(xs: List[(String, Int)]) =
    part match
      case Part.One =>
        xs
          .map(_._2)
          .filter(_ < 100000)
          .sum

      case Part.Two =>
        0

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
              ChangeDirectory(d)

        case Pat.List() =>
          List

        case Pat.Dir(d) =>
          ChildDirectory(d)

        case Pat.File(s, f) =>
          ChildFile(s.toInt, f)

  sealed trait Node(val size: Int)

  enum TerminalOutput:
    case ToRoot
    case List
    case ChildDirectory(s: String)              extends TerminalOutput with Node(0)
    case ChildFile(fileSize: Int, name: String) extends TerminalOutput with Node(fileSize)
    case ChangeDirectory(s: String)
    case GoUpDirectory

  // newtypes save me
  case class CurrentWorkingDirectory(xs: List[String]):
    def push(s: String) =
      copy(xs = xs :+ s)

    def pop =
      copy(xs = xs.slice(0, xs.size - 1))

  case class Path(xs: List[String], x: Node):
    def slug =
      "/" + xs.mkString("/")

  object CurrentWorkingDirectory:
    val empty =
      CurrentWorkingDirectory(Nil)

  def toStatelessCmd(cmd: TerminalOutput) =
    State { (cwd: CurrentWorkingDirectory) =>
      cmd match
        case TerminalOutput.ToRoot =>
          CurrentWorkingDirectory.empty -> None

        case TerminalOutput.List =>
          cwd -> None

        case d: TerminalOutput.ChildDirectory =>
          cwd -> Path(cwd.xs, d).some

        case f: TerminalOutput.ChildFile =>
          cwd -> Path(cwd.xs, f).some

        case TerminalOutput.ChangeDirectory(d) =>
          cwd.push(d) -> None

        case TerminalOutput.GoUpDirectory =>
          cwd.pop -> None
    }
