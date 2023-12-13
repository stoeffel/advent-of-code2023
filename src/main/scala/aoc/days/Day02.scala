package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import cats.syntax.all._
import aoc.implicits.all._

object Day02 extends Day[Day02.Games, Int] with Memoize:
  type Games = List[Day02.Game]

  override def parse(_part: Part)(input: String): Either[String, Games] =
    gameParser.lines.run(input)

  override def solvePart1(games: Games): Solution[Int] =
    games
      .filterNot(_.sets.exists(_.outOfBounds(GameSet(12, 13, 14))))
      .sumBy(_.id)
      .solved

  override def solvePart2(games: Games): Solution[Int] =
    games.sumBy(_.maxSet.power).solved

  case class Game(id: Int, sets: Set[GameSet]):
    def maxSet: GameSet =
      sets.foldLeft(GameSet(0, 0, 0))(_.max(_))

  enum Color:
    case Red, Green, Blue

  import Color._

  case class GameSet(red: Int, green: Int, blue: Int):
    def outOfBounds(bounds: GameSet): Boolean =
      red > bounds.red || green > bounds.green || blue > bounds.blue

    def power: Int =
      memoize[GameSet, Int] { case GameSet(red, green, blue) =>
        red * green * blue
      }(this)

    def max(other: GameSet): GameSet =
      GameSet(red.max(other.red), green.max(other.green), blue.max(other.blue))

  implicit class GameSetMapOps(map: Map[Color, Int]):
    def toGameSet: GameSet =
      GameSet(
        map.getOrElse(Red, 0),
        map.getOrElse(Green, 0),
        map.getOrElse(Blue, 0)
      )

  def gameParser: Parsley[Game] =
    for {
      _ <- string("Game")
      gameId <- (digitsInt <* char(':')).between(spaces)
      sets <- gameSetParser.sepBy1(char(';') <~> spaces)
    } yield Game(gameId, sets.toSet)

  def gameSetParser: Parsley[GameSet] =
    cubesParser
      .sepBy1(char(',') <~> spaces)
      .map(_.toMap.toGameSet)

  def cubesParser: Parsley[(Color, Int)] =
    for {
      count <- digitsInt
      _ <- spaces
      color <- colorParser
    } yield color -> count

  def colorParser: Parsley[Color] =
    string("red").as(Red)
      | string("green").as(Green)
      | string("blue").as(Blue)
