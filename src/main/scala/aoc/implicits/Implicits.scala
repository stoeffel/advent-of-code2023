package aoc.implicits

import cats.data.NonEmptyList
import scala.collection.parallel.CollectionConverters._
import scala.collection.immutable.NumericRange
import scala.collection.parallel._
import cats.instances.all._
import parsley.position.{pos => posParser}
import cats.{Foldable, Monoid}
import parsley.Parsley, Parsley._
import cats.implicits._
import parsley.character.{satisfy, endOfLine, digit, string, char}
import parsley.combinator.{
  option,
  choice,
  some,
  manyUntil => originalManyUntil,
  many => originalMany,
  sepEndBy1 => originalSepEndBy1,
  sepBy1 => originalSepBy1
}
import parsley.errors.combinator.fail
import cats.implicits._

object all:
  extension [A, B](tuple: (A, B))
    def mapFirst[C](f: A => C): (C, B) =
      val (first, second) = tuple
      (f(first), second)

    def mapSecond[C](f: B => C): (A, C) =
      val (first, second) = tuple
      (first, f(second))

    def mapBoth[C, D](f: A => C, g: B => D): (C, D) =
      val (first, second) = tuple
      (f(first), g(second))

  extension (bool: Boolean)
    def toInt: Int =
      if bool then 1 else 0

  extension [A](i: A)
    def repeat(n: Int): List[A] =
      if n == 0 then Nil
      else i :: repeat(n - 1)

  extension [A](list: List[A])
    def join(sep: A): List[A] =
      list.mapWithIndex { (a, i) =>
        if i == 0 then List(a)
        else sep :: a :: Nil
      }.flatten

    def firstAndLast: Option[(A, A)] =
      list match
        case Nil          => None
        case head :: Nil  => Some((head, head))
        case head :: tail => Some((head, tail.last))

    def first2: Option[(A, A)] =
      list match
        case a :: b :: _ => Some((a, b))
        case _           => None

    def toTuple: Option[(A, A)] =
      list match
        case a :: b :: Nil => Some(a -> b)
        case _             => None

    def toMapBy[B](f: A => B): Map[B, A] =
      list.map(a => f(a) -> a).toMap

  extension [A](seq: Seq[A])
    def mapIf(cond: A => Boolean)(f: A => A): Seq[A] =
      seq.map(a => if cond(a) then f(a) else a)

  extension [A](seq: ParSeq[A])
    def mapIf(cond: A => Boolean)(f: A => A): ParSeq[A] =
      seq.map(a => if cond(a) then f(a) else a)

  extension [A](nel: NonEmptyList[IterableOnce[A]])
    def flatten: List[A] =
      nel.toList.flatMap(_.toList)

  extension [A](nel: NonEmptyList[NonEmptyList[A]])
    def flatten: NonEmptyList[A] =
      nel.flatMap(identity)

  extension [A, B](eit: Either[A, B])
    def mapRight[C](f: B => C): Either[A, C] =
      eit match
        case Left(a)  => Left(a)
        case Right(b) => Right(f(b))

  extension [F[_], A](fa: F[A])(using F: Foldable[F])
    def sumBy[B](f: A => B)(using B: Monoid[B]): B =
      F.foldLeft(fa, B.empty)((acc, a) => B.combine(acc, f(a)))

  def notEol: Parsley[Char] =
    satisfy(_ != '\n')

  def digitInt: Parsley[Int] =
    digit.map(_.asDigit)
  def digitsInt: Parsley[Int] =
    some(digit).map(_.mkString.toInt)

  def signedDigitsInt: Parsley[Int] =
    for
      sign <- option(char('-'))
      digits <- digitsInt
    yield sign match
      case Some(_) => -digits
      case None    => digits

  def digitsLong: Parsley[Long] =
    some(digit).map(x => x.mkString.toLong)

  extension [A](p: Parsley[A])
    def run(input: String): Either[String, A] =
      p.parse(input)
        .toEither
        .leftMap(_.toString)

    def peek: Parsley[A] =
      attempt(lookAhead(p))

    def between(s: Parsley[_]): Parsley[A] =
      s *> p <* s

    def manyUntil(end: Parsley[_]): Parsley[List[A]] =
      originalManyUntil(p, end)

    def many: Parsley[List[A]] =
      originalMany(p)

    def sepEndBy1(sep: Parsley[_]): Parsley[List[A]] =
      originalSepEndBy1(p, sep)

    def sepBy1(sep: Parsley[_]): Parsley[List[A]] =
      originalSepBy1(p, sep)

    def lines: Parsley[List[A]] =
      p.sepEndBy1(endOfLine)

    def toSome: Parsley[Option[A]] =
      p.map(_.some)

    def withPos: Parsley[(A, (Int, Int))] =
      (posParser <~> p).map(_.swap)

  extension [A: Numeric](a: A)
    def dec: A =
      val numeric = implicitly[Numeric[A]]
      numeric.minus(a, numeric.one)

    def min(b: A): A =
      val numeric = implicitly[Numeric[A]]
      numeric.min(a, b)

    def max(b: A): A =
      val numeric = implicitly[Numeric[A]]
      numeric.max(a, b)

  extension (a: BigInt)
    def lcm(b: BigInt): BigInt =
      val gcd = a.gcd(b)
      a * b / gcd

  extension (a: Int)
    def even: Boolean =
      a % 2 == 0
    def toBigInt: BigInt =
      BigInt(a)

  extension (a: Seq[BigInt])
    def lcm: BigInt =
      a.reduce(_ lcm _)

  extension (a: List[BigInt])
    def lcm: BigInt =
      a.reduce(_ lcm _)

  extension [A: Numeric](r: NumericRange[A])
    def overlaps(other: NumericRange[A]): Boolean =
      val numeric = implicitly[Numeric[A]]
      numeric.compare(r.start, other.end) <= 0 && numeric.compare(
        r.end,
        other.start
      ) >= 0

    def overlap(other: NumericRange[A]): NumericRange[A] =
      val numeric = implicitly[Numeric[A]]
      val start = numeric.max(r.start, other.start)
      val end = numeric.min(r.end, other.end)
      r.copy(start = start, end = end, step = numeric.one)

    def split(
        other: NumericRange[A]
    ): (NumericRange[A], List[NumericRange[A]]) =
      val numeric = implicitly[Numeric[A]]
      val overlap = r.overlap(other)
      val nonOverlap = List(
        r.copy(
          start = r.start,
          end = numeric.minus(overlap.start, numeric.one),
          step = numeric.one
        ),
        r.copy(
          start = numeric.plus(overlap.end, numeric.one),
          end = r.end,
          step = numeric.one
        )
      ).filter(x => numeric.compare(x.start, x.end) <= 0)
      (overlap, nonOverlap)

  extension (bool: Boolean)
    def toOption[A](a: => A): Option[A] =
      if bool then Some(a) else None
