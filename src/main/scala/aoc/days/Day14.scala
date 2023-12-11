package aoc.days

import parsley.Parsley, Parsley._
import scala.collection.parallel.CollectionConverters._
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._

object Day14 extends Day[Day14.Input, Int]:
  type Input = String
