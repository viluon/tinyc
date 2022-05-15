package me.viluon.tinyc
package ir

case class IRProgram[B](fns: List[IRNode.Block[B]]) {
  def toDot: String = {
    import collection.mutable
    val numbering = mutable.Map[Any, Int]()
    def nameOf(node: Any) = "node_" + numbering.getOrElseUpdate(node, numbering.size)
    s"""digraph {
       |  ${nameOf(this)} [label="start"]
       |  ${nameOf(this)} -> ${nameOf(fns.head.body.head)}
       |${fns.map(_.toDot(nameOf)).mkString("\n").indent(2)}}""".stripMargin
  }
}
