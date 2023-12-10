package aoc

import aoc.days._
import aoc.days.Day10._
import munit.Clue.generate
import aoc.data.Vec2.all._
import aoc.data.Grid.all._

import Day10.Connection._
val example1Day10 =
  """|-L|F7
     |7S-7|
     |L|7||
     |-L-J|
     |L|-JF""".stripMargin
val expectedExample1Day10 =
  List(
    Pipe((1, 1).toVec2, EW),
    Pipe((2, 1).toVec2, NE),
    Pipe((3, 1).toVec2, NS),
    Pipe((4, 1).toVec2, SE),
    Pipe((5, 1).toVec2, SW),
    Pipe((1, 2).toVec2, SW),
    Start((2, 2).toVec2),
    Pipe((3, 2).toVec2, EW),
    Pipe((4, 2).toVec2, SW),
    Pipe((5, 2).toVec2, NS),
    Pipe((1, 3).toVec2, NE),
    Pipe((2, 3).toVec2, NS),
    Pipe((3, 3).toVec2, SW),
    Pipe((4, 3).toVec2, NS),
    Pipe((5, 3).toVec2, NS),
    Pipe((1, 4).toVec2, EW),
    Pipe((2, 4).toVec2, NE),
    Pipe((3, 4).toVec2, EW),
    Pipe((4, 4).toVec2, NW),
    Pipe((5, 4).toVec2, NS),
    Pipe((1, 5).toVec2, NE),
    Pipe((2, 5).toVec2, NS),
    Pipe((3, 5).toVec2, EW),
    Pipe((4, 5).toVec2, NW),
    Pipe((5, 5).toVec2, SE)
  ).toGrid
class Day10Part1 extends DaySuite(Day10, Part.Part1):
  testParse(
    """|.....
       |.F-7.
       |.|.|.
       |.L-J.
       |.....""".stripMargin,
    List(
      Pipe((2, 2).toVec2, SE),
      Pipe((3, 2).toVec2, EW),
      Pipe((4, 2).toVec2, SW),
      Pipe((2, 3).toVec2, NS),
      Pipe((4, 3).toVec2, NS),
      Pipe((2, 4).toVec2, NE),
      Pipe((3, 4).toVec2, EW),
      Pipe((4, 4).toVec2, NW)
    ).toGrid
  )
  testParse(
    """|.....
       |.S-7.
       |.|.|.
       |.L-J.
       |.....""".stripMargin,
    List(
      Start((2, 2).toVec2),
      Pipe((3, 2).toVec2, EW),
      Pipe((4, 2).toVec2, SW),
      Pipe((2, 3).toVec2, NS),
      Pipe((4, 3).toVec2, NS),
      Pipe((2, 4).toVec2, NE),
      Pipe((3, 4).toVec2, EW),
      Pipe((4, 4).toVec2, NW)
    ).toGrid
  )
  testParse(example1Day10, expectedExample1Day10)

  testParseAndSolve(
    """|.....
       |.S-7.
       |.|.|.
       |.L-J.
       |.....""".stripMargin,
    4
  )
  testParseAndSolve(
    """|..F7.
       |.FJ|.
       |SJ.L7
       ||F--J
       |LJ...""".stripMargin,
    8
  )
  testWithFile()
class Day10Part2 extends DaySuite(Day10, Part.Part2):
  testWithFile()
