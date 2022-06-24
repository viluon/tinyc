package me.viluon.tinyc
package analysis

import analysis.Lattice.⊤
import ir.{IRNode, IRProgram, IRRegister}

class ConstantAnalysis(ir: IRProgram[IRRegister])
  extends DataFlowAnalysis.Builder(Lattice.flatLat[Int])(ir)
    with FixpointComputation.Naive {

  def eval(env: AbstractEnv, expr: Expr): AbstractValue = expr match {
    case IRNode.KInt(_, k) => Lattice.FlatLat.Mid(k)
    case IRNode.BinOp(_, op, left, right) =>
      (env(left), env(right)) match {
        case (Lattice.FlatLat.Mid(x), Lattice.FlatLat.Mid(y)) => op.eval(x, y) match {
          case Some(r) => Lattice.FlatLat.Mid(r)
          case None => ⊤[AbstractValue] // TODO shouldn't this be bottom?
        }
        case (a, b) => a ⊔ b
      }
    case _ => ⊤[AbstractValue]
  }

  def transfer(node: Node, env: AbstractEnv): AbstractEnv = node match {
    case (expr, _) => env.updated(expr.target, eval(env, expr))
  }
}
