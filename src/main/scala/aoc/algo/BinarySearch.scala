package aoc.algo.BinarySearch

object all:
  extension (x: Long)
    def binarySearch(predicate: Long => Boolean): Long =
      def go(low: Long, high: Long): Long =
        if low > high then low
        else
          val mid = (low + high) / 2
          if predicate(mid) then go(low, mid - 1)
          else go(mid + 1, high)
      go(0, x)
