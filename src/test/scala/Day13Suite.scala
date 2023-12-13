package aoc

import aoc.days._
import aoc.days.Day13._
import munit.Clue.generate
import aoc.data.Grid.all._
import aoc.data.Vec2.all._

val example1Day13 =
  """|#.##..##.
     |..#.##.#.
     |##......#
     |##......#
     |..#.##.#.
     |..##..##.
     |#.#.##.#.
     |
     |#...##..#
     |#....#..#
     |..##..###
     |#####.##.
     |#####.##.
     |..##..###
     |#....#..#""".stripMargin

class Day13Part1 extends DaySuite(Day13, Part.Part1):
  import Day13.Surface._
  testParse(example1Day13, 2, _.length)
  testParse(example1Day13, 9, _(0)(0).length)
  testParse(example1Day13, 7, _(0).transpose.head.length)
  testParse(
    example1Day13,
    List(
      List(
        List(`#`, `.`, `#`, `#`, `.`, `.`, `#`, `#`, `.`),
        List(`.`, `.`, `#`, `.`, `#`, `#`, `.`, `#`, `.`),
        List(`#`, `#`, `.`, `.`, `.`, `.`, `.`, `.`, `#`),
        List(`#`, `#`, `.`, `.`, `.`, `.`, `.`, `.`, `#`),
        List(`.`, `.`, `#`, `.`, `#`, `#`, `.`, `#`, `.`),
        List(`.`, `.`, `#`, `#`, `.`, `.`, `#`, `#`, `.`),
        List(`#`, `.`, `#`, `.`, `#`, `#`, `.`, `#`, `.`)
      ),
      List(
        List(`#`, `.`, `.`, `.`, `#`, `#`, `.`, `.`, `#`),
        List(`#`, `.`, `.`, `.`, `.`, `#`, `.`, `.`, `#`),
        List(`.`, `.`, `#`, `#`, `.`, `.`, `#`, `#`, `#`),
        List(`#`, `#`, `#`, `#`, `#`, `.`, `#`, `#`, `.`),
        List(`#`, `#`, `#`, `#`, `#`, `.`, `#`, `#`, `.`),
        List(`.`, `.`, `#`, `#`, `.`, `.`, `#`, `#`, `#`),
        List(`#`, `.`, `.`, `.`, `.`, `#`, `.`, `.`, `#`)
      )
    )
  )

  testParse(example1Day13, 5, _(0).symetryAxis(false))
  testParse(example1Day13, 400, _(1).symetryAxis(false))
  testParseAndSolve(example1Day13, 405)
  val contents = readInputFile
  testParse(contents, 100, _(0).symetryAxis(false))
  testParse(contents, 12, _(1).symetryAxis(false))
  testParse(contents, 1000, _(2).symetryAxis(false))
  testWithFile()

class Day13Part2 extends DaySuite(Day13, Part.Part2):
  testWithFile()
