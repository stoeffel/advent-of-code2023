package aoc.days

import parsley.Parsley, Parsley._
import scala.collection.parallel.CollectionConverters._
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._

object Day13 extends Day[Day13.Patterns, Int]:
  type Patterns = List[Pattern]
  type Pattern = List[List[Surface]]
  enum Surface:
    case `.`, `#`
  import Surface._

  override def parse(part: Part)(input: String): Either[String, Patterns] =
    input.split("\n\n").toList.traverse(patternParser.run)

  override def solvePart1(patterns: Patterns): Solution[Int] =
    patterns.par.map(_.symetryAxis(false)).sum.solved

  override def solvePart2(patterns: Patterns): Solution[Int] =
    patterns.par.map(_.symetryAxis(true)).sum.solved

  extension (p: Pattern)
    def flip: Pattern = p.map(_.reverse)
    def width: Int = p.head.length
    def height: Int = p.length

    def symetryAxis(smudges: Boolean): Int =
      val vertical = p.findAxis(smudges)
      val verticalRev = p.flip.findAxis(smudges).map(_ * -1)
      val horizontal = p.transpose.findAxis(smudges)
      val horizontalRev = p.transpose.flip.findAxis(smudges).map(_ * -1)

      val v = sortedHead(vertical, verticalRev).map(toPos(p.width, _))
      val h = sortedHead(horizontal, horizontalRev).map(toPos(p.height, _))

      v orElse h.map(_ * 100) getOrElse 0

    def findAxis(smudges: Boolean): Option[Int] =
      (1 until p.width)
        .find { i =>
          val full = p.dropColumns(i)
          val (left, right) = full.leftRight
          full.width.even && left.compare(smudges)(right)
        }

    def compare(smudges: Boolean)(other: Pattern): Boolean =
      p.zip(other.flip).count(_ != _) == smudges.toInt

    def leftRight: (Pattern, Pattern) =
      p.transpose.splitAt(p.width / 2).mapBoth(_.transpose, _.transpose)

    def dropColumns(n: Int): Pattern =
      p.map(_.drop(n))

    def sortedHead(a: Option[Int], b: Option[Int]): Option[Int] =
      List(a, b).flatten.sorted.headOption

    def toPos(l: Int, x: Int): Int = (l - x.abs) / 2 + (if x > 0 then x else 0)

  def patternParser: Parsley[List[List[Surface]]] =
    choice(
      char('#').as(`#`),
      char('.').as(`.`)
    ).many.lines.map(_.filterNot(_.isEmpty))
