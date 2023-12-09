package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec
import cats.instances.all._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._
import scala.collection.parallel.CollectionConverters._

object Day09 extends Day[Day09.Input, Int]:

  override def parse(part: Part)(input: String): Either[String, Input] =
    historyParser.lines.run(input)

  override def solvePart1(input: Input): Solution[Int] =
    input.par.map(_.prediction).sum.solved
  override def solvePart2(input: Input): Solution[Int] =
    input.par.map(_.reverse.prediction).sum.solved

  type Input = List[History]

  type History = List[Int]

  extension (history: History)
    def prediction: Int =
      predictionHelper(history, List(), history.lastOption.getOrElse(0))

    @tailrec
    private def predictionHelper(
        current: History,
        diffs: History,
        acc: Int
    ): Int =
      current.first2 match
        case Some(a, b) =>
          predictionHelper(current.tail, b - a :: diffs, acc)
        case None =>
          val newAcc = acc + diffs.headOption.getOrElse(0)
          if diffs.forall(_ == 0) then newAcc
          else predictionHelper(diffs.reverse, Nil, newAcc)

  def historyParser: Parsley[History] =
    signedDigitsInt.sepBy1(spaces)
