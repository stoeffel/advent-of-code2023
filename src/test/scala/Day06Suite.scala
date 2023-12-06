package aoc

import aoc.days._
import aoc.days.Day06._
import munit.Clue.generate

val exampleDay06 =
  """|Time:      7  15   30
     |Distance:  9  40  200""".stripMargin

class Day06Part1 extends DaySuite(Day06, Part.Part1):
  val expectedFromExample = List(
    Race(7, 9),
    Race(15, 40),
    Race(30, 200)
  )
  testParse(exampleDay06, expectedFromExample)

  testSolve(expectedFromExample, 288)
  testWithFile()

class Day06Part2 extends DaySuite(Day06, Part.Part2):
  val expectedFromExample = Race(71530, 940200)
  testParse(exampleDay06, expectedFromExample)
  testSolve(expectedFromExample, 71503)
  testWithFile()
