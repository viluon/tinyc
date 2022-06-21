package me.viluon.tinyc
package ir

sealed trait IRNode[Binding] {
  def toDot(nameOf: Any => String): String = {
    val name = nameOf(this)
    this match {
      case IRNode.Block(signature, body, cont, callingConvention) =>
        val contents = body.map(nameOf(_)).sliding(2).toList.map {
          case List(x) => x
          case List(fst, snd) => s"$fst -> $snd [color=lightslateblue]"
        } ++ body.map(_.toDot(nameOf))
        s"""subgraph cluster_$name {
           |  label="BLOCK"
           |${contents.mkString("\n").indent(2)}}""".stripMargin
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
        }
    }
  }
}

object IRNode {
  case class Block[B](
                       signature: List[IRType],
                       body: List[IRExpression[B]],
                       cont: Continuation[B],
                       callingConvention: CallingConvention = CallingConvention.Unrestricted()
                     ) extends IRNode[B] {
    def successors: List[Block[B]] = body.flatMap {
      case KInt(_, _) |
           BinOp(_, _, _, _) => List()
    } ++ (cont match {
      case Continuation.Unconditional(next) => List(next)
      case Continuation.Halt() => List()
    })
  }

  sealed trait IRExpression[B] extends IRNode[B] {
    /**
     * The target binder of this expression, to which it will store the result.
     */
    def target: B
  }

  case class KInt[B](target: B, k: Int) extends IRExpression[B]
  case class BinOp[B](target: B, op: BinaryOperator, l: B, r: B) extends IRExpression[B]
}
