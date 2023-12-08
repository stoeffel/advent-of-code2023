package aoc

import aoc.days._
import aoc.days.Day08._
import munit.Clue.generate
import aoc.data.DirectedGraph.all._
import aoc.data.Zipper.all._

class Day08Part1 extends DaySuite(Day08, Part.Part1):
  import Day08.Node._
  import Day08.Instruction._
  testParse(
    """|RL
     |
     |AAA = (BBB, CCC)
     |BBB = (DDD, EEE)
     |CCC = (ZZZ, GGG)
     |DDD = (DDD, DDD)
     |EEE = (EEE, EEE)
     |GGG = (GGG, GGG)
     |ZZZ = (ZZZ, ZZZ)""".stripMargin,
    Network(
      List(Left).toZipper(Right),
      Map(
        Node("AAA") -> Vector(Node("BBB"), Node("CCC")),
        Node("BBB") -> Vector(Node("DDD"), Node("EEE")),
        Node("CCC") -> Vector(Node("ZZZ"), Node("GGG")),
        Node("DDD") -> Vector(Node("DDD"), Node("DDD")),
        Node("EEE") -> Vector(Node("EEE"), Node("EEE")),
        Node("GGG") -> Vector(Node("GGG"), Node("GGG")),
        Node("ZZZ") -> Vector(Node("ZZZ"), Node("ZZZ"))
      ).toDirectedGraph
    )
  )
  testParseAndSolve(
    """|RL
       |
       |AAA = (BBB, CCC)
       |BBB = (DDD, EEE)
       |CCC = (ZZZ, GGG)
       |DDD = (DDD, DDD)
       |EEE = (EEE, EEE)
       |GGG = (GGG, GGG)
       |ZZZ = (ZZZ, ZZZ)""".stripMargin,
    2
  )

  testParse(
    """|LLR
       |
       |AAA = (BBB, BBB)
       |BBB = (AAA, ZZZ)
       |ZZZ = (ZZZ, ZZZ)""".stripMargin,
    Network(
      List(Left, Right).toZipper(Left),
      Map(
        Node("AAA") -> Vector(Node("BBB"), Node("BBB")),
        Node("BBB") -> Vector(Node("AAA"), Node("ZZZ")),
        Node("ZZZ") -> Vector(Node("ZZZ"), Node("ZZZ")),
        Node("AAA") -> Vector(Node("BBB"), Node("BBB"))
      ).toDirectedGraph
    )
  )

  testParseAndSolve(
    """|LLR
       |
       |AAA = (BBB, BBB)
       |BBB = (AAA, ZZZ)
       |ZZZ = (ZZZ, ZZZ)""".stripMargin,
    6
  )
  testWithFile()

class Day08Part2 extends DaySuite(Day08, Part.Part2):
  testParseAndSolve(
    """|LR
       |
       |11A = (11B, XXX)
       |11B = (XXX, 11Z)
       |11Z = (11B, XXX)
       |22A = (22B, XXX)
       |22B = (22C, 22C)
       |22C = (22Z, 22Z)
       |22Z = (22B, 22B)
       |XXX = (XXX, XXX)""".stripMargin,
    6
  )
  testWithFile()
