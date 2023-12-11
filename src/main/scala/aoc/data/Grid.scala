package aoc.data.Grid

import aoc.data.Vec2.all._
import scala.collection.parallel.CollectionConverters._
import aoc.implicits.all._

object all:
  trait HasBoundingBox[A]:
    def pos(a: A): Vec2

    def maxPos(a: A): Vec2 =
      pos(a) + Vec2(width(a), height(a))

    def width(a: A): Int = 1

    def height(a: A): Int = 1

    def rangeX(a: A): Set[Int] =
      (pos(a).x to maxPos(a).x).toSet

    def rangeY(a: A): Set[Int] =
      (pos(a).y to maxPos(a).y).toSet

  extension [A](value: A)
    def pos(using bb: HasBoundingBox[A]): Vec2 =
      bb.pos(value)

    def maxPos(using bb: HasBoundingBox[A]): Vec2 =
      bb.maxPos(value)

    def width(using bb: HasBoundingBox[A]): Int =
      bb.width(value)

    def height(using bb: HasBoundingBox[A]): Int =
      bb.height(value)

    def rangeX(using bb: HasBoundingBox[A]): Set[Int] =
      bb.rangeX(value)

    def rangeY(using bb: HasBoundingBox[A]): Set[Int] =
      bb.rangeY(value)

  extension [A: HasBoundingBox](a: List[A]) def toGrid: Grid[A] = Grid(a)

  given HasBoundingBox[Vec2] with
    def pos(v: Vec2): Vec2 = v
    override def width(v: Vec2): Int = 0
    override def height(v: Vec2): Int = 0

  given HasBoundingBox[List[Vec2]] with
    override def pos(as: List[Vec2]): Vec2 =
      (as.map(_.x).min -> as.map(_.y).min).toVec2

    override def maxPos(as: List[Vec2]): Vec2 =
      (as.map(_.x).max -> as.map(_.y).max).toVec2

    override def width(as: List[Vec2]): Int =
      maxPos(as).x - pos(as).x + 1

    override def height(as: List[Vec2]): Int =
      maxPos(as).y - pos(as).y + 1

  trait Grid[A]:
    def values: List[A]

    def items: List[A]

    def neighboursOf(pos: Vec2): List[A]

    def above(pos: Vec2): Option[A]

    def below(pos: Vec2): Option[A]

    def leftOf(pos: Vec2): Option[A]

    def rightOf(pos: Vec2): Option[A]

    val rows: Iterable[Int]

    val cols: Iterable[Int]

    def allPairs: List[(A, A)]

  object Grid:
    def apply[A: HasBoundingBox](items: List[A]): Grid[A] =
      GridImpl(items)

    private case class GridImpl[A: HasBoundingBox](items: List[A])
        extends Grid[A]:
      def allPairs: List[(A, A)] =
        items
          .combinations(2)
          .toList
          .flatMap(_.first2)

      val rows: Iterable[Int] =
        items.groupBy(_.pos.y).keys

      val cols: Iterable[Int] =
        items.groupBy(_.pos.x).keys

      val grid: Map[Vec2, A] =
        items
          .flatMap(item =>
            (item.pos.x until item.pos.x + item.width)
              .flatMap(x =>
                (item.pos.y until item.pos.y + item.height)
                  .map(y => Vec2(x, y) -> item)
              )
          )
          .toMap

      def values: List[A] =
        items

      def neighboursOf(pos: Vec2): List[A] =
        pos.neighbours.flatMap(grid.get).distinctBy(_.pos)

      def above(pos: Vec2): Option[A] =
        grid.get(pos.up)

      def below(pos: Vec2): Option[A] =
        grid.get(pos.down)

      def leftOf(pos: Vec2): Option[A] =
        grid.get(pos.left)

      def rightOf(pos: Vec2): Option[A] =
        grid.get(pos.right)

    given [A: HasBoundingBox]: HasBoundingBox[Grid[A]] with
      override def pos(grid: Grid[A]): Vec2 =
        val poss = grid.items.map(_.pos)
        Vec2(poss.map(_.x).min, poss.map(_.y).min)

      override def maxPos(grid: Grid[A]): Vec2 =
        val maxPos = grid.items.map(_.maxPos)
        Vec2(maxPos.map(_.x).max, maxPos.map(_.y).max)

      override def width(grid: Grid[A]): Int =
        maxPos(grid).x - pos(grid).x

      override def height(grid: Grid[A]): Int =
        maxPos(grid).y - pos(grid).y
