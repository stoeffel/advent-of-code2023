package aoc

import aoc.days._
import aoc.days.Day07._
import munit.Clue.generate

val exampleDay07 =
  """|32T3K 765
     |T55J5 684
     |KK677 28
     |KTJJT 220
     |QQQJA 483""".stripMargin

val expectedFromExampleDay07 = List(
  Hand(List(Card.Three, Card.Two, Card.Ten, Card.Three, Card.King), 765),
  Hand(List(Card.Ten, Card.Five, Card.Five, Card.Jack, Card.Five), 684),
  Hand(List(Card.King, Card.King, Card.Six, Card.Seven, Card.Seven), 28),
  Hand(List(Card.King, Card.Ten, Card.Jack, Card.Jack, Card.Ten), 220),
  Hand(
    List(Card.Queen, Card.Queen, Card.Queen, Card.Jack, Card.Ace),
    483
  )
)
class Day07Part1 extends DaySuite(Day07, Part.Part1):
  testParse(exampleDay07, expectedFromExampleDay07)

  test("handType") {
    import HandType._
    assertEquals(expectedFromExampleDay07(0).handType, OnePair)
    assertEquals(expectedFromExampleDay07(1).handType, ThreeOfAKind)
    assertEquals(expectedFromExampleDay07(2).handType, TwoPair)
    assertEquals(expectedFromExampleDay07(3).handType, TwoPair)
    assertEquals(expectedFromExampleDay07(4).handType, ThreeOfAKind)
  }

  test("ordering") {
    assertEquals(
      expectedFromExampleDay07.sorted(using Day07.handOrd),
      List(
        expectedFromExampleDay07(0),
        expectedFromExampleDay07(3),
        expectedFromExampleDay07(2),
        expectedFromExampleDay07(1),
        expectedFromExampleDay07(4)
      )
    )
  }
  test("card ordering") {
    import Card._
    assertEquals(
      List(Four, Two, Jack, Ace, Queen).sorted(using Day07.cardOrd),
      List(Ace, Queen, Jack, Four, Two)
    )
  }

  testSolve(expectedFromExampleDay07, 6440)
  testWithFile()

class Day07Part2 extends DaySuite(Day07, Part.Part2):
  test("handType") {
    import HandType._
    assertEquals(expectedFromExampleDay07(0).jacksAreWildHandType, OnePair)
    assertEquals(expectedFromExampleDay07(1).handType, ThreeOfAKind)
    assertEquals(expectedFromExampleDay07(1).jacksAreWildHandType, FourOfAKind)
    assertEquals(expectedFromExampleDay07(2).jacksAreWildHandType, TwoPair)
    assertEquals(expectedFromExampleDay07(3).jacksAreWildHandType, FourOfAKind)
    assertEquals(expectedFromExampleDay07(4).jacksAreWildHandType, FourOfAKind)
  }

  test("card ordering") {
    import Card._
    assertEquals(
      List(Four, Two, Jack, Ace, Queen).sorted(using Day07.jackLowest),
      List(Ace, Queen, Four, Two, Jack)
    )
  }
  test("ordering") {
    assertEquals(
      expectedFromExampleDay07.sorted(using Day07.jacksAreWildOrd),
      List(
        expectedFromExampleDay07(0),
        expectedFromExampleDay07(2),
        expectedFromExampleDay07(1),
        expectedFromExampleDay07(4),
        expectedFromExampleDay07(3)
      )
    )
  }

  testSolve(expectedFromExampleDay07, 5905)
  testWithFile()
