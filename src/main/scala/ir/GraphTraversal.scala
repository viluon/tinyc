package me.viluon.tinyc
package ir

import cats.Monad
import cats.syntax.all._

/**
 * Monadically traverse a graph.
 *
 * @tparam M The monad to compute in.
 * @tparam L The label type, i.e. the type of the nodes.
 * @tparam A The type we're searching for.
 */
trait GraphTraversal[M[_], L, A] {
  def successors(node: L): M[Set[L]]
  def visit(node: L): M[Option[A]]
  def leaf(node: L): M[Option[A]]

  private final case class DfsState(visited: Set[L], stack: List[L])

  def dfs(entry: L)(implicit m: Monad[M]): M[(Set[L], Option[A])] = DfsState(Set(), List(entry)).tailRecM(dfs => dfs.stack match {
    case node :: tail =>
      // FIXME this just got unwieldy due to the short-circuiting Option.
      //  Figure out a monad transformer approach instead.
      for {
        mbRes <- visit(node)
        res <- mbRes match {
          case Some(a) => (dfs.visited, Option(a)).asRight[DfsState].pure
          case None =>
            for {
              succs <- successors(node)
              mbRes <-
                if (succs.isEmpty) leaf(node)
                else Option.empty[A].pure
              res <- mbRes match {
                case Some(a) => (dfs.visited, Option(a)).asRight[DfsState].pure
                case None =>
                  val newSuccessors = succs.removedAll(dfs.visited)
                  DfsState(dfs.visited + node, newSuccessors.toList ++ tail).asLeft[(Set[L], Option[A])].pure
              }
            } yield res
        }
      } yield res
    case Nil => (dfs.visited, Option.empty[A]).asRight[DfsState].pure
  })
}
