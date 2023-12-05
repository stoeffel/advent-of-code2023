package aoc.data.FromTo

object all:
  case class FromTo[N: Numeric](from: N, to: N):
    private val numeric = implicitly[Numeric[N]]

    def map(f: N => N): FromTo[N] =
      FromTo(f(from), f(to))

    def overlaps(ft: FromTo[N]): Boolean =
      numeric.compare(this.from, ft.to) <= 0 && numeric.compare(
        this.to,
        ft.from
      ) >= 0

    def overlap(ft: FromTo[N]): FromTo[N] =
      FromTo(numeric.max(this.from, ft.from), numeric.min(this.to, ft.to))

    def split(ft: FromTo[N]): (FromTo[N], List[FromTo[N]]) =
      val overlap = this.overlap(ft)
      val nonOverlap = List(
        FromTo(this.from, numeric.minus(overlap.from, numeric.one)),
        FromTo(numeric.plus(overlap.to, numeric.one), this.to)
      ).filter(x => numeric.compare(x.from, x.to) <= 0)
      (overlap, nonOverlap)

    def toInclusiveList: List[N] =
      numeric.plus(from, numeric.one) match
        case next if numeric.compare(next, to) > 0 =>
          List(from)
        case next =>
          from :: FromTo(next, to).toInclusiveList

    def toExclusiveList: List[N] =
      numeric.plus(from, numeric.one) match
        case next if numeric.compare(next, to) >= 0 =>
          List()
        case next =>
          from :: FromTo(next, to).toExclusiveList
