package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import cats.instances.all._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._
import aoc.data.FromTo.all._

object Day05 extends Day[Day05.Almanac, Long]:

  override def parsePart1(input: String): Either[String, Almanac] =
    almanacParser(seedsParser).run(input)

  override def parsePart2(input: String): Either[String, Almanac] =
    almanacParser(seedRangesParser).run(input)

  override def solvePart(_part: Part)(almanac: Almanac): Solution[Long] =
    almanac.seeds
      .flatMap(convert(almanac, Category.Seed, _))
      .map(_.from)
      .min
      .solved

  def convert(
      almanac: Almanac,
      category: Category,
      seedRange: SeedRange
  ): List[SeedRange] =
    category match
      case Category.Location => List(seedRange)
      case _ =>
        val Converter(to, maps) = almanac.converters(category)
        val (converted, remaining) = pipelines(maps, seedRange)
        convert(almanac, to, converted)
          ++ remaining.flatMap(convert(almanac, category, _))

  def pipelines(
      maps: List[Pipeline],
      seedRange: SeedRange
  ): (SeedRange, List[SeedRange]) =
    maps
      .map(_(seedRange))
      .collectFirst { case Some(x) => x }
      .getOrElse(seedRange -> List())

  enum Category:
    case Seed
    case Soil
    case Fertilizer
    case Water
    case Light
    case Temperature
    case Humidity
    case Location

  object Category:
    def all: List[Category] =
      List(
        Seed,
        Soil,
        Fertilizer,
        Water,
        Light,
        Temperature,
        Humidity,
        Location
      )

  case class Converter(
      to: Category,
      maps: List[Pipeline]
  )

  type SeedRange = FromTo[Long]

  type Pipeline =
    Function1[SeedRange, Option[(SeedRange, List[SeedRange])]]

  case class Almanac(
      seeds: List[SeedRange],
      converters: Map[Category, Converter]
  )

  def almanacParser(seedsP: Parsley[List[SeedRange]]): Parsley[Almanac] =
    for {
      seeds <- seedsP
      _ <- whitespaces
      converters <- converterParser.many
    } yield Almanac(seeds, converters.toMap)

  def seedsParser: Parsley[List[SeedRange]] =
    for {
      _ <- string("seeds:")
      _ <- spaces
      seeds <- digitsLong.sepBy1(spaces)
    } yield seeds.map(x => FromTo(x, x))

  def seedRangesParser: Parsley[List[SeedRange]] =
    for {
      _ <- string("seeds:")
      _ <- spaces
      seeds <- rangesParser.sepBy1(spaces)
    } yield seeds

  def rangesParser: Parsley[SeedRange] =
    for {
      start <- digitsLong
      _ <- spaces
      l <- digitsLong
    } yield FromTo(start, start + l - 1)

  def converterParser: Parsley[(Category, Converter)] =
    for {
      from <- categoryParser
      _ <- string("-to-")
      to <- categoryParser
      _ <- spaces
      _ <- string("map:")
      _ <- whitespaces
      maps <- mapParser.many
      _ <- newline | eof
    } yield (from, Converter(to, maps))

  def categoryParser: Parsley[Category] =
    choice(
      string("seed").peek *> string("seed").as(Category.Seed),
      string("soil").peek *> string("soil").as(Category.Soil),
      string("fertilizer").peek *> string("fertilizer").as(Category.Fertilizer),
      string("water").peek *> string("water").as(Category.Water),
      string("light").peek *> string("light").as(Category.Light),
      string("temperature").peek *> string("temperature").as(
        Category.Temperature
      ),
      string("humidity").peek *> string("humidity").as(Category.Humidity),
      string("location").peek *> string("location").as(Category.Location)
    )

  def mapParser: Parsley[Pipeline] =
    for {
      dest <- digitsLong
      _ <- spaces
      start <- digitsLong
      _ <- spaces
      length <- digitsLong
      _ <- newline
    } yield (toPipeline(start, length, dest))

  def toPipeline(start: Long, length: Long, dest: Long)(
      seedRange: SeedRange
  ): Option[(SeedRange, List[SeedRange])] =
    val mapRange = FromTo(start, start + length - 1)
    def convert = (x: Long) => x + dest - start
    if seedRange.overlaps(mapRange) then
      seedRange
        .split(mapRange)
        .mapFirst(_.map(convert))
        .some
    else None
