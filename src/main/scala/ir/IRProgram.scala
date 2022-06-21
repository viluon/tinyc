package me.viluon.tinyc
package ir

import cats.Applicative
import cats.syntax.all._

import scala.collection.mutable

case class IRProgram[B](start: IRNode.Block[B]) {
  def toDot: String = {
    import collection.mutable
    val numbering = mutable.Map[Any, Int]()
    def nameOf(node: Any) = "node_" + numbering.getOrElseUpdate(node, numbering.size)
    def blocks(b: IRNode.Block[B]): List[IRNode.Block[B]] = b :: (b.cont match {
      case Continuation.Unconditional(next) => blocks(next)
      case Continuation.Halt() => Nil
    })
    s"""digraph {
       |  ${nameOf(this)} [label="start"]
       |  ${nameOf(this)} -> ${nameOf(start.body.head)}
       |${blocks(start).map(_.toDot(nameOf)).mkString("\n").indent(2)}}""".stripMargin
  }

  lazy val collectBlocks: List[IRNode.Block[B]] = {
    val blocks = mutable.Buffer(start)
    var additions: mutable.Buffer[IRNode.Block[B]] = null
    do {
      additions = blocks.flatMap(_.successors).filterNot(blocks.contains)
      blocks.addAll(additions)
    } while (additions.nonEmpty)
    blocks.toList
  }

  def traverse[A, M[_]](f: IRNode.Block[B] => M[A])(implicit m: Applicative[M]): M[List[A]] = collectBlocks.traverse(f)

  def compose(other: IRProgram[B]): IRProgram[B] = ???
}
