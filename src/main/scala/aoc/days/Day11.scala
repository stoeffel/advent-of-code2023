package aoc.days

import parsley.Parsley, Parsley._
import scala.collection.parallel.CollectionConverters._
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.data.Grid.all._
import aoc.data.Vec2.all._
import aoc.implicits.all._

object Day11 extends Day[Day11.Space, BigInt]:
  override def parse(part: Part)(input: String): Either[String, Space] =
    spaceParser.run(input)

  override def solvePart1(space: Space): Solution[BigInt] =
    space.expand.distanceAllPairs.sum.solved

  override def solvePart2(space: Space): Solution[BigInt] =
    space.expand(1000000).distanceAllPairs.sum.solved

  type Space = Grid[Vec2]

  extension (space: Space)
    def distanceAllPairs: List[BigInt] =
      space.allPairs.par.map(_.manhattanDistance).toList

    def expand: Space =
      space.expand(2)

    def expand(by: Int): Space =
      space
        .expand(emptyRows, Vec2(0, by - 1), _.y)
        .expand(emptyCols, Vec2(by - 1, 0), _.x)

    def expand(empties: List[Int], by: Vec2, toPos: Vec2 => Int): Space =
      empties.foldLeft(space) { (s, e) =>
        s.items.par
          .mapIf(x => toPos(x) > e)(_.add(by))
          .toList
          .toGrid
      }

    def emptyCols: List[Int] =
      space.rangeX.removedAll(space.cols).toList.sorted.reverse

    def emptyRows: List[Int] =
      space.rangeY.removedAll(space.rows).toList.sorted.reverse

  def spaceParser: Parsley[Space] =
    choice(
      char('#').withPos.map((_, pos) => pos.swap.toVec2.some),
      char('.').as(None),
      whitespace.as(None)
    ).many.map(_.flatten.toGrid)
