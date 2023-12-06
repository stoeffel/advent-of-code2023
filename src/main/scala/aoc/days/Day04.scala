package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import cats.syntax.all._
import aoc.implicits.all._

object Day04 extends Day[Day04.Scratchcard, Int]:
  type Scratchcard = List[Card]

  case class Card(
      id: Int,
      winningNumbers: Set[Int],
      numbers: Set[Int],
      instances: Int = 1
  ):
    def winners: Int =
      winningNumbers.intersect(numbers).size

    def add(n: Int): Card =
      copy(instances = instances + n)

  override def parse(_part: Part)(input: String): Either[String, Scratchcard] =
    cardParser.lines.run(input)

  override def solvePart1(cards: Scratchcard): Solution[Int] =
    cards.map(_.winners).sumBy(1 << _ - 1).solved

  override def solvePart2(cards: Scratchcard): Solution[Int] =
    cards
      .foldLeft(Map.empty[Int, Int]) { case (acc, card) =>
        val instances = acc.getOrElse(card.id, 1)
        (1 to card.winners).foldLeft(acc + (card.id -> instances)) {
          case (acc, i) =>
            acc.updatedWith(card.id + i)(x => (x.getOrElse(1) + instances).some)
        }
      }
      .values
      .sum
      .solved

  def cardParser: Parsley[Card] =
    for {
      _ <- string("Card")
      _ <- spaces
      cardId <- digitsInt <* char(':')
      _ <- spaces
      winningNumbers <- digitsInt.sepEndBy1(spaces)
      _ <- char('|')
      _ <- spaces
      numbers <- digitsInt.sepBy1(spaces)
    } yield Card(cardId, winningNumbers.toSet, numbers.toSet)
