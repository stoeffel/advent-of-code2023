package aoc.days

import parsley.Parsley, Parsley._
import parsley.combinator._
import scala.collection.parallel.CollectionConverters._

import cats.instances.all._
import parsley.character._
import cats.implicits._
import aoc.implicits.all._
import aoc.data.Zipper.all._
import aoc.data.DirectedGraph.all._

object Day08 extends Day[Day08.Network, BigInt]:

  case class Network(
      instructions: Zipper[Instruction],
      graph: DirectedGraph[Node]
  ):
    def pathLength(start: Node, endPredicate: Node => Boolean): BigInt =
      graph
        .path(
          start,
          (_: Zipper[Instruction], x: Node) => endPredicate(x),
          instructions
        ) { (instructions, _, neighbours) =>
          (neighbours(instructions.focus.ordinal), instructions.nextCycle)
        }
        .length
        .dec
        .toBigInt

  case class Node(name: String)

  enum Instruction:
    case Left, Right

  import Node._
  import Instruction._

  override def parse(part: Part)(input: String): Either[String, Network] =
    networkParser.run(input)

  override def solvePart1(input: Network): Solution[BigInt] =
    input.pathLength(Node("AAA"), _.name == "ZZZ").solved

  override def solvePart2(input: Network): Solution[BigInt] =
    input.graph.nodes
      .filter(_.name.endsWith("A"))
      .par
      .map(input.pathLength(_, _.name.endsWith("Z")))
      .toVector
      .lcm
      .solved

  def networkParser: Parsley[Network] =
    for
      focused <- instructionParser
      instructions <- instructionParser.many
      _ <- whitespaces
      nodes <- nodeParser.lines
    yield Network(instructions.toZipper(focused), nodes.toMap.toDirectedGraph)

  def instructionParser: Parsley[Instruction] =
    choice(
      char('L').as(Left),
      char('R').as(Right)
    )

  def nodeParser: Parsley[(Node, Vector[Node])] =
    for
      node <- identifierParser
      _ <- string(" = ")
      _ <- char('(')
      left <- identifierParser
      _ <- char(',')
      _ <- spaces
      right <- identifierParser
      _ <- char(')')
    yield (node -> Vector(left, right))

  def identifierParser: Parsley[Node] =
    choice(
      letterOrDigit.many.map(_.mkString).map(Node(_))
    )
