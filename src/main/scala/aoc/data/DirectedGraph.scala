package aoc.data.DirectedGraph

import cats.{Foldable, Eval}
import cats.instances.set._
import cats.syntax.foldable._
import scala.annotation.tailrec

object all:
  trait DirectedGraph[A]:
    def nodes: Vector[A]

    def edges: Vector[(A, A)]

    def neighbours: Map[A, Vector[A]]

    def path[B](
        from: A,
        predicate: A => Boolean,
        state: B
    )(step: (B, A, Vector[A]) => (A, B)): List[A]

  object DirectedGraph:
    def apply[A](nodes: Vector[A], edges: Vector[(A, A)]): DirectedGraph[A] =
      DirectedGraphImpl(
        nodes,
        edges,
        edges.groupMap(_._1)(_._2)
      )

    private case class DirectedGraphImpl[A](
        nodes: Vector[A],
        edges: Vector[(A, A)],
        neighbours: Map[A, Vector[A]]
    ) extends DirectedGraph[A]:

      def path[B](from: A, predicate: A => Boolean, state: B)(
          step: (B, A, Vector[A]) => (A, B)
      ): List[A] =
        @tailrec
        def pathHelper(from: A, state: B, acc: List[A]): List[A] =
          if (predicate(from)) acc.reverse
          else
            step(state, from, neighbours(from)) match
              case (next, newState) => pathHelper(next, newState, from :: acc)

        pathHelper(from, state, List(from))

  extension [A](xs: Map[A, Vector[A]])
    def toDirectedGraph: DirectedGraph[A] =
      DirectedGraph(
        xs.keys.toVector,
        xs.toVector.flatMap { case (a, bs) =>
          bs.map(b => (a, b))
        }
      )
