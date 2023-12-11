package aoc

import aoc.days._
import aoc.days.Day11._
import munit.Clue.generate
import aoc.data.Vec2.all._
import aoc.data.Grid.all._

val example1Day11 =
  """|...#......
       |.......#..
       |#.........
       |..........
       |......#...
       |.#........
       |.........#
       |..........
       |.......#..
       |#...#.....""".stripMargin
class Day11Part1 extends DaySuite(Day11, Part.Part1):
  testParse(
    example1Day11,
    List(
      (4, 1).toVec2,
      (8, 2).toVec2,
      (1, 3).toVec2,
      (7, 5).toVec2,
      (2, 6).toVec2,
      (10, 7).toVec2,
      (8, 9).toVec2,
      (1, 10).toVec2,
      (5, 10).toVec2
    ).toGrid
  )

  testParse(example1Day11, Set(1, 2, 3, 5, 6, 7, 9, 10), _.rows)
  testParse(example1Day11, (10, 10).toVec2, _.maxPos)
  testParse(example1Day11, List(4, 8).reverse, _.emptyRows)
  testParse(example1Day11, Set(1, 2, 4, 5, 7, 8, 10), _.cols)
  testParse(example1Day11, List(3, 6, 9).reverse, _.emptyCols)
  val expanded =
    """|....#........
       |.........#...
       |#............
       |.............
       |.............
       |........#....
       |.#...........
       |............#
       |.............
       |.............
       |.........#...
       |#....#.......""".stripMargin

  val expectedExpanded = List(
    (5, 1).toVec2,
    (10, 2).toVec2,
    (1, 3).toVec2,
    (9, 6).toVec2,
    (2, 7).toVec2,
    (13, 8).toVec2,
    (10, 11).toVec2,
    (1, 12).toVec2,
    (6, 12).toVec2
  ).toGrid

  testParse(expanded, expectedExpanded)
  testParse(example1Day11, expectedExpanded, _.expand)

  test("distance") {
    assertEquals(
      (expectedExpanded.items(0), expectedExpanded.items(6)).manhattanDistance,
      BigInt(15)
    )
    assertEquals(
      (expectedExpanded.items(2), expectedExpanded.items(5)).manhattanDistance,
      BigInt(17)
    )
    assertEquals(
      (expectedExpanded.items(7), expectedExpanded.items(8)).manhattanDistance,
      BigInt(5)
    )
  }
  test("allPairs") {
    given HasBoundingBox[Int] with
      override def pos(a: Int): Vec2 = (a, a).toVec2
      override def maxPos(a: Int): Vec2 = (a, a).toVec2

    assertEquals(
      List(1, 2, 3, 4).toGrid.allPairs,
      List(
        (1, 2),
        (1, 3),
        (1, 4),
        (2, 3),
        (2, 4),
        (3, 4)
      )
    )
    assertEquals(
      List(1, 2, 3).toGrid.allPairs,
      List(
        (1, 2),
        (1, 3),
        (2, 3)
      )
    )
  }
  test("distanceAllPairs") {
    assertEquals(
      List(
        (1, 1).toVec2,
        (2, 2).toVec2,
        (3, 3).toVec2
      ).toGrid.distanceAllPairs,
      List[BigInt](2, 4, 2)
    )

  }
  testParseAndSolve(example1Day11, 374)
  val contents = readInputFile
  testParse(contents, (140, 140).toVec2, _.maxPos)
  testParse(contents, List(18, 28, 70, 78, 94, 97).reverse, _.emptyRows)
  testParse(contents, List(16, 54, 61, 73, 89, 95).reverse, _.emptyCols)
  testParse(contents, (146, 146).toVec2, _.expand.maxPos)
  testParse(contents, 451, _.items.size)
  testWithFile()
class Day11Part2 extends DaySuite(Day11, Part.Part2):
  testParse(example1Day11, 1030, _.expand(10).distanceAllPairs.sum)
  testWithFile()
