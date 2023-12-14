package aoc.days

import parsley.Parsley, Parsley._
import scala.annotation.tailrec
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._

object Day14 extends Day[Day14.Platform, Int] with Memoize:
  type Platform = List[List[Rock]]

  enum Rock:
    case Free
    case Empty
    case Fix

  import Rock._

  given Ordering[Rock] with
    override def compare(x: Rock, y: Rock): Int =
      x.ordinal - y.ordinal

  override def parse(part: Part)(input: String): Either[String, Platform] =
    platformParser.run(input)

  override def solvePart1(platform: Platform): Solution[Int] =
    platform.tilt.totalLoad.solved

  override def solvePart2(platform: Platform): Solution[Int] =
    platform.cicle(1000000000).totalLoad.solved

  extension (platform: Platform)
    def totalLoad: Int =
      platform
        .flatMap(_.reverse.zipWithIndex.collect { case (Free, i) => i + 1 })
        .sum

    @tailrec
    def cicle(n: Int, history: History = History.empty): Platform =
      history ? platform match
        case Some(first) => history.at(n, first)
        case None =>
          platform.tiltAnticlockwise.cicle(n, platform :: history)

    def tiltAnticlockwise: Platform =
      Function.repeat[Platform](4)(_.tilt.rotateLeft)(platform)

    def tilt: Platform = platform.map(_.tiltCol)
    def rotateLeft: Platform = platform.transpose.reverse

    def render: String =
      (0 until platform.head.length)
        .map { y =>
          (0 until platform.length).map { x =>
            platform(x)(y) match
              case Free  => 'O'
              case Fix   => '#'
              case Empty => '.'
          }.mkString
        }
        .mkString("\n")

  extension (col: List[Rock])
    def tiltCol: List[Rock] =
      memoize[List[Rock], List[Rock]] { col =>
        col
          .splitted(Fix)
          .map(_.sorted)
          .join(List(Fix))
          .flatten
      }(col)

  case class History(counter: Int, history: List[(Platform, Int)]):
    def ?(platform: Platform): Option[Int] =
      history.find(_._1 == platform).map(_._2)

    def ::(p: Platform): History =
      History(counter + 1, (p, counter) :: history)

    def at(n: Int, first: Int): Platform =
      val index = (n - first) % (counter - first) + first
      history.reverse(index)._1

  object History:
    def empty: History = History(0, Nil)

  def platformParser: Parsley[Platform] =
    rockParser.lines.map(_.filter(_.nonEmpty).transpose)

  def rockParser: Parsley[List[Rock]] =
    choice(
      char('#').as(Fix),
      char('O').as(Free),
      char('.').as(Empty)
    ).many
