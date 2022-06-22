package me.viluon.tinyc
package ir

import cats.Applicative
import cats.syntax.all._

import scala.collection.mutable

case class IRProgram[B](entry: IRNode.BasicBlockID, blocks: Map[IRNode.BasicBlockID, IRNode.Block[B]]) {
  def toDot: DotGraph = {
    import collection.mutable
    val numbering = mutable.Map[Any, Int]()
    def nameOf(node: Any) = "node_" + numbering.getOrElseUpdate(node, numbering.size)
    def successors(b: IRNode.BasicBlockID): List[IRNode.BasicBlockID] = b :: (blocks(b).cont match {
      case Continuation.Branch(_, consequent, alternative) => successors(consequent.callee) ++ successors(alternative.callee)
      case Continuation.Unconditional(next) => successors(next.callee)
      case Continuation.Return(_) => Nil
      case Continuation.Halt() => Nil
    })

    val body = successors(entry).map(blocks(_)).map(_.toDot(nameOf))
    DotGraph.Digraph(
      Set(nameOf(this) -> Map("shape" -> "diamond", "label" -> "start")) ++ body.map {
        case DotGraph.Subgraph(name, nodes, edges, props) =>
          DotGraph.Subgraph(name, nodes, Set(), props).toString -> Map[String, String]()
      },
      Set((nameOf(this), "entry_" + nameOf(blocks(entry).id), Map("color" -> "lightslateblue"))) ++ body.flatMap(_.edges)
    )
  }

  def display(): Unit = {
    // run the Bash script to display the IR
    val cmd = s"dot -Tpng -v | feh -"
    val proc = Runtime.getRuntime.exec(Array("bash", "-c", cmd))
    proc.getOutputStream.write(toDot.toString.getBytes)
    proc.getOutputStream.close()
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
