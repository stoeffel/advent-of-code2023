package aoc.days

import cats.syntax.all._
import parsley.Parsley, Parsley._
import parsley.combinator._
import parsley.character.{digit, char, string, item, endOfLine, satisfy}
import cats.implicits._
import aoc.implicits.all._

object Day01 extends Day[List[List[Int]], Int]:
  type Input = List[List[Int]]

  override def parsePart1(input: String): Either[String, Input] =
    choice(digitInt.toSome, notEol.as(None)).many
      .map(_.flatten)
      .lines
      .run(input)

  override def parsePart2(input: String): Either[String, Input] =
    choice(digitWords.toSome, digitInt.toSome, notEol.as(None)).many
      .map(_.flatten)
      .lines
      .run(input)

  override def solvePart(_part: Part)(input: Input): Solution[Int] =
    input
      .flatMap(_.firstAndLast)
      .sumBy(10 * _ + _)
      .solved

  private val digitWords: Parsley[Int] =
    choice(
      string("zero").peek *> item.as(0),
      string("one").peek *> item.as(1),
      string("two").peek *> item.as(2),
      string("three").peek *> item.as(3),
      string("four").peek *> item.as(4),
      string("five").peek *> item.as(5),
      string("six").peek *> item.as(6),
      string("seven").peek *> item.as(7),
      string("eight").peek *> item.as(8),
      string("nine").peek *> item.as(9)
    )
