package aoc

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}
import aoc.days._

class Day01Part1 extends DaySuite(Day01, Part.Part1):
  testParse("", List(Nil))
  testParse("1", List(List(1)))
  testParse("1\n2", List(List(1), List(2)))
  testParse("1\n2\nab3c\nab4cd5e", List(List(1), List(2), List(3), List(4, 5)))
  testParse(
    """1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet""",
    List(List(1, 2), List(3, 8), List(1, 2, 3, 4, 5), List(7))
  )

  testSolve(List(List(1, 2)), 12)
  testSolve(List(List(1, 2, 3)), 13)
  testSolve(List(List(1, 3, 2), List(3, 2, 1)), 43)
  testSolve(
    List(List(1, 2), List(3, 8), List(1, 2, 3, 4, 5), List(7)),
    142
  )
  testWithFile()

class Day01Part2 extends DaySuite(Day01, Part.Part2):
  testParse(
    """two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen""",
    List(
      List(2, 1, 9),
      List(8, 2, 3),
      List(1, 2, 3),
      List(2, 1, 3, 4),
      List(4, 9, 8, 7, 2),
      List(1, 8, 2, 3, 4),
      List(7, 6)
    )
  )
  testWithFile()
