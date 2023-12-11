import aoc.days._
import os._

@main def run: Unit =
  val days =
    (1 to 25).map(n => s"day${n.toString.reverse.padTo(2, '0').reverse}")
  // clear screen
  println("\u001b[2J\u001b[0;0H")
  println(s"Day  Stars  Times")
  println(s"------------Part1, Part2--")
  println(s"---------------------------")
  val times = days.flatMap(runDay)

  // sparkline of all times
  val max = times.map(_._3).max
  val min = times.map(_._3).min
  val range = max - min
  val sparkline = times.map { case (d, p, t) =>
    val n = (((t - min) / range.toDouble) * 7).toInt
    val spark = "▁▂▃▄▅▆▇█" (n)
    val dStr =
      if p == 2 then "  "
      else d.toString.reverse.padTo(2, '0').reverse
    List(spark, dStr(0), dStr(1), "¹²" (p - 1))
  }.transpose

  println(s"---------------------------")
  sparkline match
    case Vector(
          spark: Vector[String],
          d1: Vector[String],
          d2: Vector[String],
          p: Vector[String]
        ) =>
      println(s"Time:  ${spark.mkString("")}")
      println(s"Part:  ${p.mkString("")}")
      println(s"Day:   ${d1.mkString("")}")
      println(s"       ${d2.mkString("")}")
    case _ => ()

  println(s"---------------------------")
  println(s"\n\n\n")

def runDay(day: String): List[(Int, Int, Long)] =
  import Part._
  val (stars, times) = List(Part1, Part2).map(runPart(day, _)).unzip
  val diff = times.map(t => s"${t.toString.reverse.padTo(3, ' ').reverse}ms")
  val dayNr = day.replace("day", "")
  println(
    s"${dayNr}: ${stars.mkString("")}   (${diff.mkString(", ")})"
  )
  times.zipWithIndex.map((t, i) => (dayNr.toInt, i + 1, t))

def runPart(dayStr: String, part: Part): (String, Long) =
  val maybeDay = dayStr match
    case "day01" => Right(Day01)
    case "day02" => Right(Day02)
    case "day03" => Right(Day03)
    case "day04" => Right(Day04)
    case "day05" => Right(Day05)
    case "day06" => Right(Day06)
    case "day07" => Right(Day07)
    case "day08" => Right(Day08)
    case "day09" => Right(Day09)
    case "day10" => Right(Day10)
    case "day11" => Right(Day11)
    case "day12" => Right(Day12)
    case "day13" => Right(Day13)
    case "day14" => Right(Day14)
    case "day15" => Right(Day15)
    case "day16" => Right(Day16)
    case "day17" => Right(Day17)
    case "day18" => Right(Day18)
    case "day19" => Right(Day19)
    case "day20" => Right(Day20)
    case "day21" => Right(Day21)
    case "day22" => Right(Day22)
    case "day23" => Right(Day23)
    case "day24" => Right(Day24)
    case "day25" => Right(Day25)
    case _       => Left("💀")
  maybeDay match
    case Left(error) => (error, 0)
    case Right(day: Day[_, _]) =>
      val input = os.read(os.pwd / "input" / s"$dayStr.txt")
      val start = System.currentTimeMillis()
      val parsed = day.parse(part)(input)
      parsed match
        case Left("Not implemented") => ("🎁", 0)
        case Left(error)             => throw new Exception(error)
        case Right(parsed) =>
          val result = day.solvePart(part)(parsed)
          val end = System.currentTimeMillis()
          // compare with output file
          val outputFilename = s"day${day.toString}part${part.toString}"
          val expected =
            os.read(os.pwd / "output" / s"$outputFilename.txt")
          result match
            case Solution.Unsolved(msg) => ("🚧", 0)
            case Solution.Solved(value) =>
              if value.toString == expected then ("🌟", end - start)
              else ("💀", end - start)
