package me.viluon.tinyc
package ir

import cats.Applicative
import cats.syntax.all._

import scala.collection.mutable

case class IRProgram[B](entry: IRNode.BasicBlockID, blocks: Map[IRNode.BasicBlockID, IRNode.Block[B]]) {
  def toDot: String = {
    import collection.mutable
    val numbering = mutable.Map[Any, Int]()
    def nameOf(node: Any) = "node_" + numbering.getOrElseUpdate(node, numbering.size)
    def successors(b: IRNode.BasicBlockID): List[IRNode.BasicBlockID] = b :: (blocks(b).cont match {
      case Continuation.Branch(_, consequent, alternative) => successors(consequent.callee) ++ successors(alternative.callee)
      case Continuation.Unconditional(next) => successors(next.callee)
      case Continuation.Halt() => Nil
    })
    s"""digraph {
       |  ${nameOf(this)} [label="start"]
       |  ${nameOf(this)} -> ${nameOf(blocks(entry).body.head)}
       |${successors(entry).map(blocks(_)).map(_.toDot(nameOf)).mkString("\n").indent(2)}}""".stripMargin
  }

  lazy val linearizedBlocks: List[IRNode.Block[B]] = {
    val seq = mutable.Buffer(entry)
    var additions: mutable.Buffer[IRNode.BasicBlockID] = null
    do {
      additions = seq.flatMap(blocks(_).successors).filterNot(seq.contains)
      seq.addAll(additions)
    } while (additions.nonEmpty)
    seq.map(blocks(_)).toList
  }

  def traverse[A, M[_]](f: IRNode.Block[B] => M[A])(implicit m: Applicative[M]): M[List[A]] = linearizedBlocks.traverse(f)
}
