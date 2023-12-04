package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import cats.syntax.all._
import aoc.implicits.all._
import aoc.data.Grid.all._
import aoc.data.Vec2.all._

object Day03 extends Day[Day03.EngineSchematic, Int]:

  override def parse(_part: Part)(
      input: String
  ): Either[String, EngineSchematic] =
    engineSchematicParser.run(input)

  override def solvePart1(engineSchematic: EngineSchematic): Solution[Int] =
    engineSchematic.symbols
      .flatMap(_.adjacent(engineSchematic))
      .sumBy(_.value)
      .solved

  override def solvePart2(engineSchematic: EngineSchematic): Solution[Int] =
    engineSchematic.symbols
      .map(_.toEnginePart(engineSchematic))
      .sumBy(_.ratio)
      .solved

  type EngineSchematic = Grid[Number | Symbol]

  extension (grid: EngineSchematic)
    def symbols: List[Symbol] =
      grid.values.collect { case s: Symbol => s }

  case class Symbol(name: Char, pos: Vec2):
    def adjacent(grid: EngineSchematic): List[Number] =
      grid.neighboursOf(pos).collect { case n: Number => n }

    def toEnginePart(grid: EngineSchematic): EnginePart =
      adjacent(grid).toTuple match
        case Some(a, b) =>
          EnginePart.Gear(a.value, b.value)

        case None =>
          EnginePart.Magic

  enum EnginePart:
    case Gear(partNumber1: Int, partNumber2: Int)
    case Magic

  extension (part: EnginePart)
    def ratio: Int =
      part match
        case EnginePart.Gear(partNumber1, partNumber2) =>
          partNumber1 * partNumber2

        case EnginePart.Magic =>
          0

  case class Number(value: Int, pos: Vec2)

  given HasBoundingBox[Number | Symbol] with
    def pos(nos: Number | Symbol): Vec2 =
      nos match
        case number: Number => number.pos
        case symbol: Symbol => symbol.pos

    override def width(nos: Number | Symbol): Int =
      nos match
        case number: Number => number.value.toString.length
        case symbol: Symbol => 1

  def engineSchematicParser: Parsley[EngineSchematic] =
    lineParser.lines.map(_.flatten).map(Grid(_))

  def lineParser: Parsley[List[Number | Symbol]] =
    choice(
      numberParser.toSome,
      symbolParser.toSome,
      char('.').as(None)
    ).many.map(_.flatten)

  def numberParser: Parsley[Number] =
    digitsInt.withPos.map { case (value, pos) =>
      Number(value, pos.swap.toVec2)
    }

  def symbolParser: Parsley[Symbol] =
    satisfy(x => !x.isWhitespace && x != '.').withPos
      .map { case (name, pos) =>
        Symbol(name, pos.swap.toVec2)
      }
