package aoc

import aoc.days._
import aoc.days.Day09._
import munit.Clue.generate

val example1Day09 =
  """|0 3 6 9 12 15
|1 3 6 10 15 21
|10 13 16 21 30 45""".stripMargin
val expectedExample1Day09 =
  List(
    List(0, 3, 6, 9, 12, 15),
    List(1, 3, 6, 10, 15, 21),
    List(10, 13, 16, 21, 30, 45)
  )
class Day09Part1 extends DaySuite(Day09, Part.Part1):

  testParse(
    example1Day09,
    expectedExample1Day09
  )

  testSolve(List(expectedExample1Day09(0)), 18)
  testSolve(List(expectedExample1Day09(1)), 28)
  testSolve(List(expectedExample1Day09(2)), 68)

  testParseAndSolve(example1Day09, 114)
  testWithFile()

class Day09Part2 extends DaySuite(Day09, Part.Part2):
  testSolve(List(expectedExample1Day09(0)), -3)
  testSolve(List(expectedExample1Day09(1)), 0)
  testSolve(List(expectedExample1Day09(2)), 5)
  testParseAndSolve(example1Day09, 2)
  testWithFile()
