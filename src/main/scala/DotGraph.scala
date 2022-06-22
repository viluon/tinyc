package me.viluon.tinyc


sealed trait DotGraph {
  val nodes: Set[DotGraph.Node]
  val edges: Set[DotGraph.Edge]

  lazy val body: String = {
    import DotGraph.stringifyProps

    nodes.toList.map {
      case (name, props) => name + stringifyProps(props)
    }.sorted.mkString("\n") + "\n" + edges.toList.map {
      case (from, to, props) => s"$from -> $to " + stringifyProps(props)
    }.sorted.mkString("\n")
  }
}

object DotGraph {
  type Props = Map[String, String]
  type Node = (String, Props)
  type Edge = (String, String, Props)

  def stringifyProps(props: DotGraph.Props, withBraces: Boolean = true): String = {
    val (l, r) = if (withBraces) " [" -> "]" else " " -> ""
    props.map { case (k, v) => s"$k=\"$v\"" }.mkString(l, " ", r)
  }

  case class Digraph(nodes: Set[Node], edges: Set[Edge] = Set()) extends DotGraph {
    override def toString: String = s"digraph {\n" + body.indent(2) + "}"
  }

  case class Subgraph(name: String, nodes: Set[Node], edges: Set[Edge], props: Props) extends DotGraph {
    override def toString: String =
      s"""subgraph $name {
         |${stringifyProps(props, withBraces = false)}
         |${body.indent(2)}}""".stripMargin
  }
}
