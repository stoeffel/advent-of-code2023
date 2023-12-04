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

  extension (tuple: (Int, Int))
    def toVec2: Vec2 =
      Vec2(tuple._1, tuple._2)
