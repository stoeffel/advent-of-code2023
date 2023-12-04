package aoc

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}
import aoc.days._
import aoc.days.Day03._
import aoc.data.Grid.all._
import aoc.data.Vec2.all._
import munit.Clue.generate

val expectedFromExampleDay03 = Grid(
  List(
    Number(467, Vec2(1, 1)),
    Number(114, Vec2(6, 1)),
    Symbol('*', Vec2(4, 2)),
    Number(35, Vec2(3, 3)),
    Number(633, Vec2(7, 3)),
    Symbol('#', Vec2(7, 4)),
    Number(617, Vec2(1, 5)),
    Symbol('*', Vec2(4, 5)),
    Symbol('+', Vec2(6, 6)),
    Number(58, Vec2(8, 6)),
    Number(592, Vec2(3, 7)),
    Number(755, Vec2(7, 8)),
    Symbol('$', Vec2(4, 9)),
    Symbol('*', Vec2(6, 9)),
    Number(664, Vec2(2, 10)),
    Number(598, Vec2(6, 10))
  )
)

class Day03Part1 extends DaySuite(Day03, Part.Part1):
  testParse(
    """|467..114..""".stripMargin,
    Grid(
      List(
        Number(467, Vec2(1, 1)),
        Number(114, Vec2(6, 1))
      )
    )
  )
  testParse(
    """|467..114..
       |...X......
       |..........
       |.........*
       |......""".stripMargin,
    Grid(
      List(
        Number(467, Vec2(1, 1)),
        Number(114, Vec2(6, 1)),
        Symbol('X', Vec2(4, 2)),
        Symbol('*', Vec2(10, 4))
      )
    )
  )
  testParse(
    """|467..114..
       |...*......
       |..35..633.
       |......#...
       |617*......
       |.....+.58.
       |..592.....
       |......755.
       |...$.*....
       |.664.598..""".stripMargin,
    expectedFromExampleDay03
  )
  testSolve(expectedFromExampleDay03, 4361)
  testWithFile()

class Day03Part2 extends DaySuite(Day03, Part.Part2):
  testSolve(expectedFromExampleDay03, 467835)
  testWithFile()
