package aoc.data.Vec2

object all:
  case class Vec2(x: Int, y: Int):
    def toTuple: (Int, Int) =
      (x, y)

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

    def +(other: Vec2): Vec2 =
      Vec2(this.x + other.x, this.y + other.y)

  extension (tuple: (Int, Int))
    def toVec2: Vec2 =
      Vec2(tuple._1, tuple._2)
