package aoc

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}
import aoc.days._
import aoc.days.Day02._
import aoc.days.Day02.Color._

val expectedFromExampleDay02 =
  List(
    Game(
      1,
      Set(
        Map(Blue -> 3, Red -> 4).toGameSet,
        Map(Red -> 1, Green -> 2, Blue -> 6).toGameSet,
        Map(Green -> 2).toGameSet
      )
    ),
    Game(
      2,
      Set(
        Map(Blue -> 1, Green -> 2).toGameSet,
        Map(Green -> 3, Blue -> 4, Red -> 1).toGameSet,
        Map(Green -> 1, Blue -> 1).toGameSet
      )
    ),
    Game(
      3,
      Set(
        Map(Green -> 8, Blue -> 6, Red -> 20).toGameSet,
        Map(Blue -> 5, Red -> 4, Green -> 13).toGameSet,
        Map(Green -> 5, Red -> 1).toGameSet
      )
    ),
    Game(
      4,
      Set(
        Map(Green -> 1, Red -> 3, Blue -> 6).toGameSet,
        Map(Green -> 3, Red -> 6).toGameSet,
        Map(Green -> 3, Blue -> 15, Red -> 14).toGameSet
      )
    ),
    Game(
      5,
      Set(
        Map(Red -> 6, Blue -> 1, Green -> 3).toGameSet,
        Map(Blue -> 2, Red -> 1, Green -> 2).toGameSet
      )
    )
  )
class Day02Part1 extends DaySuite(Day02, Part.Part1):
  testParse(
    """|Game 1:   3 blue,  4 green, 2 red""".stripMargin,
    List(
      Game(1, Set(Map(Blue -> 3, Green -> 4, Red -> 2).toGameSet))
    )
  )
  testParse(
    """|Game 1: 3 blue, 4 green, 2 red
       |Game 2: 5 blue, 2 green, 1 red""".stripMargin,
    List(
      Game(1, Set(Map(Blue -> 3, Green -> 4, Red -> 2).toGameSet)),
      Game(2, Set(Map(Blue -> 5, Green -> 2, Red -> 1).toGameSet))
    )
  )
  testParse(
    """|Game 1: 3 blue, 4 green, 2 red; 2 blue, 3 green, 1 red
       |Game 2: 5 blue, 2 green, 1 red; 1 blue, 2 green, 3 red""".stripMargin,
    List(
      Game(
        1,
        Set(
          Map(Blue -> 3, Green -> 4, Red -> 2).toGameSet,
          Map(Blue -> 2, Green -> 3, Red -> 1).toGameSet
        )
      ),
      Game(
        2,
        Set(
          Map(Blue -> 5, Green -> 2, Red -> 1).toGameSet,
          Map(Blue -> 1, Green -> 2, Red -> 3).toGameSet
        )
      )
    )
  )
  testParse(
    """|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
       |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
       |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
       |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
       |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin,
    expectedFromExampleDay02
  )

  testSolve(expectedFromExampleDay02, 8)
  testWithFile()

class Day02Part2 extends DaySuite(Day02, Part.Part2):
  testSolve(expectedFromExampleDay02, 2286)
  testWithFile()
