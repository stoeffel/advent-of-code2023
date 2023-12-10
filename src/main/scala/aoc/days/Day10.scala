package aoc.days

import parsley.Parsley, Parsley._
import java.awt.geom._
import scala.collection.parallel.CollectionConverters._
import parsley.combinator._
import parsley.character._
import cats.implicits._
import aoc.data.Grid.all._
import aoc.data.Vec2.all._
import aoc.implicits.all._
import aoc.data.DirectedGraph.all._

object Day10 extends Day[Day10.Input, Int]:

  override def parse(part: Part)(input: String): Either[String, Input] =
    pipeMazeParser.run(input)

  override def solvePart(part: Part)(input: Input): Solution[Int] =
    val path = input.start
      .map(_.walkMaze(input))
      .getOrElse(Nil)
    part match
      case Part.Part1 => (path.length / 2).solved
      case Part.Part2 =>
        fillPath(path).par.filterNot(input.items.contains).length.solved

  import Connection._

  type Input = Grid[Tile]

  extension (grid: Grid[Tile])
    def start: Option[Start] =
      grid.items.collectFirst { case s: Start => s }

  type Tile = Start | Pipe

  case class Start(pos: Vec2)

  case class Pipe(pos: Vec2, connection: Connection)

  extension (tile: Tile)
    def pipeOption: Option[Pipe] =
      tile match
        case p: Pipe => Some(p)
        case _       => None

  enum Connection:
    case NS // |
    case EW // -
    case NE // L
    case NW // J
    case SW // 7
    case SE // F

  extension (connection: Connection)
    def north: Boolean =
      connection match
        case NS | NE | NW => true
        case _            => false

    def south: Boolean =
      connection match
        case NS | SW | SE => true
        case _            => false

    def east: Boolean =
      connection match
        case EW | NE | SE => true
        case _            => false

    def west: Boolean =
      connection match
        case EW | NW | SW => true
        case _            => false

  given HasBoundingBox[Tile] with
    def pos(p: Tile): Vec2 =
      p match
        case Start(pos)   => pos
        case Pipe(pos, _) => pos

  extension (start: Start)
    def walkMaze(grid: Input): List[Vec2] =
      gridToGraph(grid).path[Set[Vec2]](
        start.pos,
        _.contains(start.pos) && _ == start.pos,
        Set.empty
      ) { (visited, current, neighbours) =>
        neighbours.filterNot(visited.contains(_)) match
          case head +: tail =>
            (head, visited + current)

          case _ =>
            (start.pos, visited)
      }

    private def gridToGraph(grid: Input): DirectedGraph[Vec2] =
      grid.items.par
        .map {
          case Pipe(pos, connection) =>
            connection match
              case NS => (pos, pos.up :: pos.down :: Nil)
              case EW => (pos, pos.left :: pos.right :: Nil)
              case NE => (pos, pos.up :: pos.right :: Nil)
              case NW => (pos, pos.up :: pos.left :: Nil)
              case SW => (pos, pos.down :: pos.left :: Nil)
              case SE => (pos, pos.down :: pos.right :: Nil)

          case Start(pos) =>
            (
              pos,
              List(
                grid
                  .above(pos)
                  .flatMap(_.pipeOption)
                  .flatMap(_.connection.south.toOption(pos.up)),
                grid
                  .below(pos)
                  .flatMap(_.pipeOption)
                  .flatMap(_.connection.north.toOption(pos.down)),
                grid
                  .leftOf(pos)
                  .flatMap(_.pipeOption)
                  .flatMap(_.connection.west.toOption(pos.left)),
                grid
                  .rightOf(pos)
                  .flatMap(_.pipeOption)
                  .flatMap(_.connection.east.toOption(pos.right))
              ).flatten
            )
        }
        .toList
        .toMap
        .toDirectedGraph

  def fillPath(path: List[Vec2]): List[Vec2] =
    val min = path.pos
    val max = path.maxPos
    val path2D = new Path2D.Double()
    path match
      case Nil => path2D
      case head :: tail =>
        path2D.moveTo(head.x, head.y)
        tail.foreach { case Vec2(x, y) =>
          path2D.lineTo(x, y)
        }
    path2D.closePath()

    (min.x to max.x).par
      .flatMap { x =>
        (min.y to max.y).par.map { y =>
          path2D
            // A little hack to not include points on the edges of the path
            .contains(Rectangle2D.Double(x - 0.1, y - 0.1, 0.2, 0.2))
            .toOption(Vec2(x, y))
        }
      }
      .toList
      .flatten

  def pipeMazeParser: Parsley[Input] =
    choice(
      startParser.toSome,
      pipeParser.toSome,
      char('.').as(None)
    ).many.map(_.flatten).lines.map(_.flatten.toGrid)

  def startParser: Parsley[Start] =
    char('S').withPos.map(_._2.swap.toVec2).map(Start(_))

  def pipeParser: Parsley[Pipe] =
    connectionParser.withPos.map((connection, pos) =>
      Pipe(pos.swap.toVec2, connection)
    )

  def connectionParser: Parsley[Connection] =
    choice(
      char('|').as(NS),
      char('-').as(EW),
      char('L').as(NE),
      char('J').as(NW),
      char('7').as(SW),
      char('F').as(SE)
    )
