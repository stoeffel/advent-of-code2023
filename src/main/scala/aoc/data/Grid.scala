package aoc.data.Grid

import aoc.data.Vec2.all._

object all:
  trait HasBoundingBox[A]:
    def pos(a: A): Vec2

    def width(a: A): Int = 1

    def height(a: A): Int = 1

  extension [A](value: A)
    def pos(using bb: HasBoundingBox[A]): Vec2 =
      bb.pos(value)

    def width(using bb: HasBoundingBox[A]): Int =
      bb.width(value)

    def height(using bb: HasBoundingBox[A]): Int =
      bb.height(value)

  case class Grid[A: HasBoundingBox](items: List[A]):
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
