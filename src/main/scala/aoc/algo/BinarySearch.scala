package aoc.algo.BinarySearch

object all:
  extension [A: Integral](x: A)
    def binarySearch(predicate: A => Boolean)(using
        A: Integral[A]
    ): A =
      import A._
      var (min, max) = (zero, x)
      while (min < max)
        val mid = (min + max) / fromInt(2)
        if predicate(mid) then max = mid else min = mid + one
      min
