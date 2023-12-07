package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._
import aoc.algo.BinarySearch.all._

object Day07 extends Day[Day07.CamelCards, Int]:

  import math.Ordered.orderingToOrdered

  override def parse(part: Part)(input: String): Either[String, CamelCards] =
    camelCardParser.lines.run(input)

  override def solvePart(part: Part)(input: CamelCards): Solution[Int] =
    val gameOrd = part match
      case Part.Part1 => handOrd
      case Part.Part2 => jacksAreWildOrd
    input
      .sorted(using gameOrd)
      .zipWithIndex
      .sumBy { (hand, i) => hand.bid * (i + 1) }
      .solved

  type CamelCards = List[Hand]

  import HandType._
  import Card._

  case class Hand(hand: List[Card], bid: Int):
    def handType: HandType =
      fromCards(hand)

    def jacksAreWildHandType: HandType =
      fromCards(hand.filterNot(_ == Jack)) + hand.count(_ == Jack)

    def fromCards(xs: List[Card]): HandType =
      xs
        .groupBy(identity)
        .toSeq
        .map(_._2.size)
        .sorted(Ordering[Int].reverse) match
        case 5 :: _      => FiveOfAKind
        case 4 :: _      => FourOfAKind
        case 3 :: 2 :: _ => FullHouse
        case 3 :: _      => ThreeOfAKind
        case 2 :: 2 :: _ => TwoPair
        case 2 :: _      => OnePair
        case _           => HighCard

  given handOrd: Ordering[Hand] with
    override def compare(x: Hand, y: Hand): Int =
      given Ordering[Card] = summon[cardOrd.type]
      y.handType.compare(x.handType) match
        case 0 => y.hand.compare(x.hand)
        case x => x

  given jacksAreWildOrd: Ordering[Hand] with
    override def compare(x: Hand, y: Hand): Int =
      given Ordering[Card] = summon[jackLowest.type]
      y.jacksAreWildHandType.compare(x.jacksAreWildHandType) match
        case 0 => y.hand.compare(x.hand)
        case x => x

  enum Card:
    case Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four,
      Three, Two

  given cardOrd: Ordering[Card] with
    override def compare(x: Card, y: Card): Int =
      x.ordinal.compare(y.ordinal)

  given jackLowest: Ordering[Card] with
    // Ordinal, but jack are always last
    override def compare(x: Card, y: Card): Int =
      (x, y) match
        case (Jack, Jack) => 0
        case (Jack, _)    => 1
        case (_, Jack)    => -1
        case _            => x.ordinal.compare(y.ordinal)

  enum HandType:
    case FiveOfAKind
    case FourOfAKind
    case FullHouse
    case ThreeOfAKind
    case TwoPair
    case OnePair
    case HighCard

  extension (handType: HandType)
    def +(n: Int): HandType =
      import HandType._
      if n == 0 then handType
      else
        handType match
          case FourOfAKind  => FiveOfAKind
          case ThreeOfAKind => FourOfAKind + (n - 1)
          case TwoPair      => FullHouse
          case OnePair      => ThreeOfAKind + (n - 1)
          case HighCard     => OnePair + (n - 1)
          case _            => handType

  given Ordering[HandType] with
    override def compare(x: HandType, y: HandType): Int =
      x.ordinal.compare(y.ordinal)

  def camelCardParser: Parsley[Hand] =
    for
      hand <- cardParser.many
      _ <- spaces
      bid <- digitsInt
    yield Hand(hand, bid)

  def cardParser: Parsley[Card] =
    choice(
      string("A").as(Ace),
      string("K").as(King),
      string("Q").as(Queen),
      string("J").as(Jack),
      string("T").as(Ten),
      string("9").as(Nine),
      string("8").as(Eight),
      string("7").as(Seven),
      string("6").as(Six),
      string("5").as(Five),
      string("4").as(Four),
      string("3").as(Three),
      string("2").as(Two)
    )
