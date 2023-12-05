package aoc

import java.io.{File, PrintWriter, BufferedWriter, FileWriter}
import cats.data.NonEmptyList
import aoc.days._
import aoc.implicits.all._

class DaySuite[A, B] extends TestSuite {
  var day: Day[A, B] = null
  var part: Part = null
  def this(day: Day[A, B], part: Part) = {
    this()
    this.day = day
    this.part = part
  }

  def testParse(input: String, expected: A) =
    test(s"test parser") {
      val obtained = day.parse(part)(input)
      assertRight(obtained, expected)
    }

  def testParse[C](input: String, expected: C, f: A => C) =
    test(s"test parser") {
      val obtained = day.parse(part)(input)
      assertRight(obtained.mapRight(f), expected)
    }
  def testSolve(input: A, expected: B) =
    test(s"test solver") {
      val obtained = day.solvePart(part)(input)
      assertEquals(obtained, expected.solved)
    }

  def testParseAndSolve(input: String, expected: B) =
    test(s"test parser and solver") {
      val obtained = day.parse(part)(input)
      obtained match
        case Left(error) => assert(false, error)
        case Right(parsed) => {
          val obtained = day.solvePart(part)(parsed)
          assertEquals(obtained, expected.solved)
        }
    }

  // test for part of a day input file and output file
  def testWithFile() =
    test(s"solves") {
      // read input file
      val filename = s"day${day.toString}"
      val input = io.Source.fromFile(s"input/${filename}.txt").mkString
      val parsed = day.parse(part)(input)
      parsed match
        case Left(error) => assert(false, error)
        case Right(parsed) => {
          val obtained = day.solvePart(part)(parsed)
          // write output file
          val outputFilename = s"day${day.toString}part${part.toString}"
          obtained match
            case Solution.Unsolved => assert(false, "solution not found")
            case Solution.Solved(value) => {
              val output = value.toString
              // create and write output file
              val outputFile = new File(s"output/${outputFilename}.txt")
              outputFile.getParentFile().mkdirs()
              // does output file exist?
              if (outputFile.exists) {
                val outputFromFile =
                  io.Source.fromFile(s"output/${outputFilename}.txt").mkString
                assertEquals(outputFromFile, output)
              } else {
                val bw = new BufferedWriter(new FileWriter(outputFile))
                bw.write(output)
                bw.close()
              }

              // assert file exists
              val file = new File(s"output/${outputFilename}.txt")
              assert(file.exists)
            }
        }
    }

}
