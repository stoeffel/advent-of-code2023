package aoc.implicits

import cats.data.NonEmptyList
import cats.instances.all._
import parsley.position.{pos => posParser}
import cats.{Foldable, Monoid}
import parsley.Parsley, Parsley._
import cats.implicits._
import parsley.character.{satisfy, endOfLine, digit, string, char}
import parsley.combinator.{
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

  extension [A](list: List[A])
    def firstAndLast: Option[(A, A)] =
      list match
        case Nil          => None
        case head :: Nil  => Some((head, head))
        case head :: tail => Some((head, tail.last))

    def toTuple: Option[(A, A)] =
      list match
        case a :: b :: Nil => Some(a -> b)
        case _             => None

    def toMapBy[B](f: A => B): Map[B, A] =
      list.map(a => f(a) -> a).toMap

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
