package aoc

import aoc.days._
import aoc.days.Day12._
import munit.Clue.generate

val example1Day12 =
  """|???.### 1,1,3
     |.??..??...?##. 1,1,3
     |?#?#?#?#?#?#?#? 1,3,1,6
     |????.#...#... 4,1,1
     |????.######..#####. 1,6,5
     |?###???????? 3,2,1""".stripMargin

import State._
val parsedExample1Day12 =
  List(
    Record(List(`?`, `?`, `?`, `.`, `#`, `#`, `#`), List(1, 1, 3)),
    Record(
      List(
        `.`,
        `?`,
        `?`,
        `.`,
        `.`,
        `?`,
        `?`,
        `.`,
        `.`,
        `.`,
        `?`,
        `#`,
        `#`,
        `.`
      ),
      List(1, 1, 3)
    ),
    Record(
      List(
        `?`,
        `#`,
        `?`,
        `#`,
        `?`,
        `#`,
        `?`,
        `#`,
        `?`,
        `#`,
        `?`,
        `#`,
        `?`,
        `#`,
        `?`
      ),
      List(1, 3, 1, 6)
    ),
    Record(
      List(
        `?`,
        `?`,
        `?`,
        `?`,
        `.`,
        `#`,
        `.`,
        `.`,
        `.`,
        `#`,
        `.`,
        `.`,
        `.`
      ),
      List(4, 1, 1)
    ),
    Record(
      List(
        `?`,
        `?`,
        `?`,
        `?`,
        `.`,
        `#`,
        `#`,
        `#`,
        `#`,
        `#`,
        `#`,
        `.`,
        `.`,
        `#`,
        `#`,
        `#`,
        `#`,
        `#`,
        `.`
      ),
      List(1, 6, 5)
    ),
    Record(
      List(
        `?`,
        `#`,
        `#`,
        `#`,
        `?`,
        `?`,
        `?`,
        `?`,
        `?`,
        `?`,
        `?`,
        `?`
      ),
      List(3, 2, 1)
    )
  )

class Day12Part1 extends DaySuite(Day12, Part.Part1):
  testParse(example1Day12, parsedExample1Day12)
  testSolve(List(parsedExample1Day12(0)), 1)
  testSolve(List(parsedExample1Day12(1)), 4)
  testSolve(List(parsedExample1Day12(2)), 1)
  testSolve(List(parsedExample1Day12(3)), 1)
  testSolve(List(parsedExample1Day12(4)), 4)
  testSolve(List(parsedExample1Day12(5)), 10)
  testParseAndSolve(example1Day12, 21)
  testWithFile()

class Day12Part2 extends DaySuite(Day12, Part.Part2):
  testWithFile()
