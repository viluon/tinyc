package me.viluon.tinyc
package ir

import DotGraph.{Digraph, Subgraph}

sealed trait IRNode[Binding] {
  def toDot(nameOf: Any => String): DotGraph = {
    val name = nameOf(this)
    this match {
      case IRNode.Block(id, params, env, body, cont, callingConvention) =>
        def controlFlowEdge(a: String, b: String, props: DotGraph.Props = Map()): DotGraph.Edge =
          (a, b, Map("color" -> "lightslateblue") ++ props)
        val name = nameOf(id)
        val spine = body.map(nameOf(_)).sliding(2).toList.flatMap {
          case List(_) => List()
          case List(fst, snd) => List(controlFlowEdge(fst, snd))
        }
        val contents = body.map(_.toDot(nameOf)).reduceOption[DotGraph] {
          case (a, b) => Digraph(a.nodes union b.nodes, a.edges union b.edges)
        }.getOrElse(Digraph(Set()))

        val blockKind = callingConvention match {
          case CallingConvention.Function() => "func"
          case CallingConvention.Unrestricted() => "block"
        }
        val signature = params.mkString("(", ", ", ")")
        val envDict = env.map(p => p._1 + ": " + p._2).mkString(", ")
        val firstEdge = body.headOption.map(h => controlFlowEdge(s"entry_$name", nameOf(h)))
        val outEdges = body.lastOption.map(last => {
          val contType = cont match {
            case Continuation.Unconditional(_) => "jmp"
            case Continuation.Branch(_, _, _) => "br"
            case Continuation.Return(_) => "ret"
            case Continuation.Halt() => "halt"
          }
          cont.targets.zipWithIndex.map {
            case (target, i) => controlFlowEdge(nameOf(last), "entry_" + nameOf(target.callee), Map(
              "label" -> (contType + i + target.args.mkString(": ", ", ", "")),
              "fontsize" -> "10.0",
            ))
          }
        }).iterator.toSet.flatten
        val entryNode = s"entry_$name" -> Map("shape" -> "point", "label" -> "")

        Subgraph(s"cluster_$name", contents.nodes + entryNode, outEdges ++ firstEdge ++ spine ++ contents.edges, Map(
          "label" -> s"(${id.n}) $blockKind$signature with {$envDict}"
        ))

      case expr: IRNode.IRExpression[Binding] => expr match {
        case IRNode.KInt(target, k) =>
          Digraph(Set(name -> Map("label" -> s"$target ← KInt $k")))
        case IRNode.BinOp(target, op, l, r) =>
          Digraph(Set(
            name -> Map("label" -> s"$target ← BinOp($op)"),
            nameOf(l) -> Map("label" -> l.toString),
            nameOf(r) -> Map("label" -> r.toString),
          ), Set(
            (nameOf(l), name, Map()),
            (nameOf(r), name, Map()),
          ))
        case IRNode.Copy(target, source) =>
          Digraph(Set(
            name -> Map("label" -> s"$target ← $source"),
            nameOf(source) -> Map("label" -> source.toString),
          ), Set(
            (nameOf(source), name, Map()),
          ))
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
