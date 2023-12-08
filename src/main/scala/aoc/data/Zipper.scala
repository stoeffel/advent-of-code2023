package aoc.data.Zipper

object all:
  trait Zipper[A]:
    def focus: A

    def nextCycle: Zipper[A]

  object Zipper:
    def apply[A](focus: A, right: Seq[A]): Zipper[A] =
      ZipperImpl(Nil, focus, right)

    private case class ZipperImpl[A](left: Seq[A], focus: A, right: Seq[A])
        extends Zipper[A]:
      def nextCycle: Zipper[A] = this match
        case ZipperImpl(left, focus, Nil) =>
          ZipperImpl(Nil, left.last, left.init :+ focus)
        case ZipperImpl(left, focus, right) =>
          ZipperImpl(focus +: left, right.head, right.tail)

      def toSeq: Seq[A] = left.reverse ++ (focus +: right)

  extension [A](xs: Seq[A])
    def toZipper(focus: A): Zipper[A] = Zipper(focus, xs)
