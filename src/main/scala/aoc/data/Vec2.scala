package aoc.data.Vec2

object all:
  case class Vec2(x: Int, y: Int):
    def toTuple: (Int, Int) =
      (x, y)

    def swap: Vec2 =
      Vec2(y, x)

    def neighbours: List[Vec2] =
      List(
        this.copy(x = this.x - 1),
        this.copy(x = this.x + 1),
        this.copy(y = this.y - 1),
        this.copy(y = this.y + 1),
        this.copy(x = this.x - 1, y = this.y - 1),
        this.copy(x = this.x - 1, y = this.y + 1),
        this.copy(x = this.x + 1, y = this.y - 1),
        this.copy(x = this.x + 1, y = this.y + 1)
      )

    def up: Vec2 =
      this.copy(y = this.y - 1)

    def down: Vec2 =
      this.copy(y = this.y + 1)

    def left: Vec2 =
      this.copy(x = this.x - 1)

    def right: Vec2 =
      this.copy(x = this.x + 1)

    def add(other: Vec2): Vec2 =
      this + other

    def +(other: Vec2): Vec2 =
      Vec2(this.x + other.x, this.y + other.y)

    def manhattanDistance(other: Vec2): Int =
      (this.x - other.x).abs + (this.y - other.y).abs

  given Ordering[Vec2] with
    def compare(a: Vec2, b: Vec2): Int =
      if a.y == b.y then a.x - b.x
      else a.y - b.y

  extension (tuple: (Int, Int))
    def toVec2: Vec2 =
      Vec2(tuple._1, tuple._2)

  extension (pair: (Vec2, Vec2))
    def manhattanDistance: BigInt =
      pair._1.manhattanDistance(pair._2)
