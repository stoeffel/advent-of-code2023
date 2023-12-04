package aoc

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}
import aoc.days._
import aoc.days.Day04._

val expectedFromExampleDay04 = List(
  Card(1, Set(41, 48, 83, 86, 17), Set(83, 86, 6, 31, 17, 9, 48, 53)),
  Card(2, Set(13, 32, 20, 16, 61), Set(61, 30, 68, 82, 17, 32, 24, 19)),
  Card(3, Set(1, 21, 53, 59, 44), Set(69, 82, 63, 72, 16, 21, 14, 1)),
  Card(4, Set(41, 92, 73, 84, 69), Set(59, 84, 76, 51, 58, 5, 54, 83)),
  Card(5, Set(87, 83, 26, 28, 32), Set(88, 30, 70, 12, 93, 22, 82, 36)),
  Card(6, Set(31, 18, 13, 56, 72), Set(74, 77, 10, 23, 35, 67, 36, 11))
)

class Day04Part1 extends DaySuite(Day04, Part.Part1):
  testParse(
    """|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53""".stripMargin,
    List(
      Card(1, Set(41, 48, 83, 86, 17), Set(83, 86, 6, 31, 17, 9, 48, 53))
    )
  )
  testParse(
    """|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
       |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
       |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
       |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
       |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
       |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin,
    expectedFromExampleDay04
  )
  testSolve(List(expectedFromExampleDay04.head), 8)

  testSolve(expectedFromExampleDay04, 13)
  testWithFile()

class Day04Part2 extends DaySuite(Day04, Part.Part2):
  testSolve(expectedFromExampleDay04, 30)
  testWithFile()
