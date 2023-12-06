package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._
import aoc.algo.BinarySearch.all._

object Day06 extends Day[Day06.Input, Long]:

  override def parsePart1(input: String): Either[String, Races] =
    racesParser.run(input)

  override def parsePart2(input: String): Either[String, Race] =
    raceParser.run(input)

  override def solvePart(part: Part)(input: Input): Solution[Long] =
    input match
      case (races: Races) => races.map(_.possibleWinners).product.solved
      case (race: Race)   => race.possibleWinners.solved

  type Input = Races | Race
  type Races = List[Race]

  case class Race(time: Long, distance: Long):
    def possibleWinners: Long =
      max - min + 1

    private def min: Long =
      time.binarySearch(distance(_) > distance)

    private def max: Long =
      time - time.binarySearch(x => distance(time - x) > distance)

    private def distance(buttonPress: Long): Long =
      (time - buttonPress) * buttonPress

  def racesParser: Parsley[Races] =
    for {
      times <- string("Time:") *> spaces *> digitsLong.sepBy1(spaces)
      _ <- whitespaces
      distances <- string("Distance:") *> spaces *> digitsLong.sepBy1(spaces)
    } yield times.zip(distances).map(Race(_, _))

  def raceParser: Parsley[Race] =
    for {
      time <- string("Time:") *> spaces *> digitsLong
        .sepBy1(spaces)
        .map(_.mkString.toLong)
      _ <- whitespaces
      distance <- string("Distance:") *> spaces *> digitsLong
        .sepBy1(spaces)
        .map(_.mkString.toLong)
    } yield Race(time, distance)
