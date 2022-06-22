package me.viluon.tinyc
package ir

sealed trait IRNode[Binding] {
  def toDot(nameOf: Any => String): String = {
    val name = nameOf(this)
    this match {
      case IRNode.Block(id, params, env, body, cont, callingConvention) =>
        def controlFlowEdge(a: String, b: String): String = s"$a -> $b [color=lightslateblue]"
        val name = nameOf(id)
        val contents = body.map(nameOf(_)).sliding(2).toList.map {
          case List(x) => x
          case List(fst, snd) => controlFlowEdge(fst, snd)
        } ++ body.map(_.toDot(nameOf))

        val blockKind = callingConvention match {
          case CallingConvention.Function() => "func"
          case CallingConvention.Unrestricted() => "block"
        }
        val signature = params.mkString("(", ", ", ")")
        val envDict = env.map(p => p._1 + ": " + p._2).mkString(", ")
        val outEdges = body.lastOption.map(last => cont.callees
          .map(id => controlFlowEdge(nameOf(last), "entry_" + nameOf(id)))
          .mkString("\n").indent(2)
        ).getOrElse("")
        val firstEdge = body.headOption.map(h => controlFlowEdge(s"entry_$name", nameOf(h))).getOrElse("")

        s"""subgraph cluster_$name {
           |  label="(${id.n}) $blockKind$signature with {$envDict}"
           |  entry_$name [shape=diamond label=""]
           |  $firstEdge
           |${contents.mkString("\n").indent(2)}
           |$outEdges}""".stripMargin

      case expr: IRNode.IRExpression[Binding] =>
        def foo(deps: Any*) = {
          s"""${nameOf(expr.target)} <- $name
             |${deps.map(nameOf(_) + " -> " + name).mkString("\n")}
             |""".stripMargin
        }

        expr match {
          case IRNode.KInt(target, k) => s"$name [label=\"$target ← KInt $k\"]\n"
          case IRNode.BinOp(target, op, l, r) =>
            s"""$name [label="$target ← BinOp($op)"]
               |${nameOf(l)} [label="$l"]
               |${nameOf(r)} [label="$r"]
               |${nameOf(l)} -> $name
               |${nameOf(r)} -> $name
               |""".stripMargin
          case IRNode.Copy(target, source) =>
            s"""$name [label="$target ← $source"]
               |${nameOf(source)} [label="$source"]
               |${nameOf(source)} -> $name
               |""".stripMargin
        }
    }
  }
}

object IRNode {
  case class BasicBlockID(n: Int) // TODO maybe we want to add the origin?

  case class Block[B](
                       id: BasicBlockID,
                       params: List[IRType],
                       env: Map[String, B],
                       body: List[IRExpression[B]],
                       cont: Continuation[B, BasicBlockID],
                       callingConvention: CallingConvention = CallingConvention.Unrestricted()
                     ) extends IRNode[B] {
    def successors: List[BasicBlockID] = body.flatMap {
      case KInt(_, _) |
           BinOp(_, _, _, _) |
           Copy(_, _) => List()
    } ++ cont.callees
  }

  sealed trait IRExpression[B] extends IRNode[B] {
    /**
     * The target binder of this expression, to which it will store the result.
     */
    def target: B
  }

  case class KInt[B](target: B, k: Int) extends IRExpression[B]
  case class BinOp[B](target: B, op: BinaryOperator, l: B, r: B) extends IRExpression[B]
  case class Copy[B](target: B, source: B) extends IRExpression[B]
}
