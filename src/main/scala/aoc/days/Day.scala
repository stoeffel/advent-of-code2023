package aoc.days

import cats._

enum Part:
  case Part1
  case Part2
  override def toString: String = this match
    case Part1 => "1"
    case Part2 => "2"

trait IsPart[A <: Part]:
  def part: Part

object PartIsPart:
  given IsPart[Part.Part1.type] with
    def part = Part.Part1

  given IsPart[Part.Part2.type] with
    def part = Part.Part2

import PartIsPart._

enum Solution[+A] derives CanEqual:
  case Solved(value: A)
  case Unsolved

implicit val solutionFunctor: Functor[Solution] = new Functor[Solution]:
  def map[A, B](fa: Solution[A])(f: A => B): Solution[B] =
    fa match
      case Solution.Solved(value) => Solution.Solved(f(value))
      case Solution.Unsolved      => Solution.Unsolved

implicit class SolutionOps[A](result: A):
  def solved: Solution[A] = Solution.Solved(result)

trait Day[A, B]:
  override def toString: String =
    val dayStr = this.getClass.getSimpleName.substring(3, 5)
    s"$dayStr"

  def parse(part: Part)(input: String): Either[String, A] =
    part match
      case Part.Part1 => parsePart1(input)
      case Part.Part2 => parsePart2(input)

  def parsePart1(input: String): Either[String, A] =
    Left("Not implemented")

  def parsePart2(input: String): Either[String, A] =
    Left("Not implemented")

  def solvePart(part: Part)(input: A): Solution[B] =
    part match
      case Part.Part1 => solvePart1(input)
      case Part.Part2 => solvePart2(input)

  def solvePart1(input: A): Solution[B] =
    Solution.Unsolved

  def solvePart2(input: A): Solution[B] =
    Solution.Unsolved
