package aoc

import aoc.days._
import aoc.days.Day14._
import munit.Clue.generate

val example1Day14 =
  """|O....#....
     |O.OO#....#
     |.....##...
     |OO.#O....O
     |.O.....O#.
     |O.#..O.#.#
     |..O..#O..O
     |.......O..
     |#....###..
     |#OO..#....""".stripMargin

val example1Day14expected =
  """|OOOO.#.O..
     |OO..#....#
     |OO..O##..O
     |O..#.OO...
     |........#.
     |..#....#.#
     |..O..#.O.O
     |..O.......
     |#....###..
     |#....#....""".stripMargin

class Day14Part1 extends DaySuite(Day14, Part.Part1):
  import Rock._
  testParse(
    """|.
       |O""".stripMargin,
    """|O
       |.""".stripMargin,
    _.tilt.render
  )
  testParse(
    """|..#.O
       |O....
       |.OO.O
       |..#..
       |.....
       |..O..""".stripMargin,
    """|OO#.O
       |..O.O
       |.....
       |..#..
       |..O..
       |.....""".stripMargin,
    _.tilt.render
  )
  testParse(example1Day14, example1Day14expected, _.tilt.render)
  testParseAndSolve(example1Day14, 136)
  testWithFile()

class Day14Part2 extends DaySuite(Day14, Part.Part2):
  import Rock._
  testParseAndSolve(example1Day14, 64)
  testWithFile()
