package aoc.data.Grid

import aoc.data.Vec2.all._

object all:
  trait HasBoundingBox[A]:
    def pos(a: A): Vec2

    def maxPos(a: A): Vec2 =
      pos(a) + Vec2(width(a), height(a))

    def width(a: A): Int = 1

    def height(a: A): Int = 1

  extension [A](value: A)
    def pos(using bb: HasBoundingBox[A]): Vec2 =
      bb.pos(value)

    def maxPos(using bb: HasBoundingBox[A]): Vec2 =
      bb.maxPos(value)

    def width(using bb: HasBoundingBox[A]): Int =
      bb.width(value)

    def height(using bb: HasBoundingBox[A]): Int =
      bb.height(value)

  extension [A: HasBoundingBox](a: List[A]) def toGrid: Grid[A] = Grid(a)

  given HasBoundingBox[List[Vec2]] with
    override def pos(as: List[Vec2]): Vec2 =
      (as.map(_.x).min -> as.map(_.y).min).toVec2

    override def width(as: List[Vec2]): Int =
      as.map(_.x).max - as.map(_.x).min + 1

    override def height(as: List[Vec2]): Int =
      as.map(_.y).max - as.map(_.y).min + 1

  trait Grid[A]:
    def values: List[A]

    def items: List[A]

    def neighboursOf(pos: Vec2): List[A]

    def above(pos: Vec2): Option[A]

    def below(pos: Vec2): Option[A]

    def leftOf(pos: Vec2): Option[A]

    def rightOf(pos: Vec2): Option[A]

    val rows: Map[Int, List[A]]

    val cols: Map[Int, List[A]]

  object Grid:
    def apply[A: HasBoundingBox](items: List[A]): Grid[A] =
      GridImpl(items)

    private case class GridImpl[A: HasBoundingBox](items: List[A])
        extends Grid[A]:

      val rows: Map[Int, List[A]] =
        items.groupBy(_.pos.y)

      val cols: Map[Int, List[A]] =
        items.groupBy(_.pos.x)

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
        grid.items.map(_.pos).minBy(_.toTuple)

      override def maxPos(grid: Grid[A]): Vec2 =
        grid.items.map(_.maxPos).maxBy(_.toTuple)

      override def width(grid: Grid[A]): Int =
        maxPos(grid).x - pos(grid).x

      override def height(grid: Grid[A]): Int =
        maxPos(grid).y - pos(grid).y
