package aoc.days

import parsley.Parsley, Parsley._
import scala.collection.parallel.CollectionConverters._
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._

object Day12 extends Day[Day12.SpringReport, BigInt] with Memoize:
  import State._
  type SpringReport = List[Record]

  override def parse(part: Part)(input: String): Either[String, SpringReport] =
    recordParser.lines.run(input)

  override def solvePart1(report: SpringReport): Solution[BigInt] =
    report.par
      .map(_.countPermutations)
      .sum
      .solved

  override def solvePart2(report: SpringReport): Solution[BigInt] =
    report.par
      .map(_.unfold(5).countPermutations)
      .sum
      .solved

  enum State:
    case `.`, `#`, `?`

  case class Record(springs: List[State], groups: List[Int]):
    def unfold(n: Int): Record =
      Record(
        springs.repeat(n).join(List(`?`)).flatten,
        groups.repeat(n).flatten
      )

    def countPermutations: BigInt =
      memoize[Record, BigInt] { case Record(springs, groups) =>
        springs match
          case Nil => groups.isEmpty.toInt
          case `.` :: tail =>
            Record(tail, groups).countPermutations
          case `?` :: tail =>
            Record(tail, groups).countPermutations +
              Record(`#` :: tail, groups).countPermutations
          case `#` :: tail =>
            groups match
              case Nil => 0
              case group :: _
                  if springs.length < group || springs
                    .take(group)
                    .exists(_ == `.`) =>
                0
              case group :: Nil =>
                Record(springs.drop(group), Nil).countPermutations
              case group :: _ :: _ =>
                if springs.length < group + 1 || springs(group) == `#` then 0
                else
                  Record(springs.drop(group + 1), groups.tail).countPermutations
      }(this)

  def recordParser: Parsley[Record] =
    for
      springs <- stateParser.many
      _ <- spaces
      groups <- digitsInt.sepBy1(char(','))
    yield Record(springs, groups)

  def stateParser: Parsley[State] =
    choice(
      char('.').as(`.`),
      char('#').as(`#`),
      char('?').as(`?`)
    )
