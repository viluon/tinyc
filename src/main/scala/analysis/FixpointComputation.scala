package me.viluon.tinyc
package analysis

import ir.{IRNode, IRProgram, IRRegister}

trait FixpointComputation { self: DataFlowAnalysis =>
  def fixpoint(program: IRProgram[IRRegister], initialState: ProgramState): ProgramState
  def fixpoint(): ProgramState
}

object FixpointComputation {
  trait Naive extends FixpointComputation { self: DataFlowAnalysis =>
    private implicit def pl: Lattice[ProgramState] = programLat
    import Lattice.{LatOps, ⊥}

    def fixpoint(program: IRProgram[IRRegister], initialState: ProgramState = programLat.bot): ProgramState = {
      def join(node: Node, state: BlockState)(implicit cfg: IRNode.Block[IRRegister], nl: Lattice[AbstractEnv]): AbstractEnv = {
        val predecessors = cfg.body.take(node._2) // FIXME only valid for forward analysis,
                                                  //  though I guess inverting the IR just reverses block bodies
        predecessors.zipWithIndex.foldLeft(⊥[AbstractEnv]) {
          case (env, pred) => env ⊔ state(pred)
        }
      }

      def step(prevProgState: ProgramState): ProgramState = program.blocks.foldLeft(prevProgState) {
        case (progState, (blockID, cfg)) =>
          progState.updated(blockID, cfg.body.zipWithIndex.foldLeft(progState(blockID)) { case (state, node) =>
            implicit val nl: Lattice[AbstractEnv] = nodeLat(node)
            state.updated(node, state(node) ⊔ transfer(node, join(node, state)(cfg, nl)))
          })
      }

      var curr = initialState
      var last = curr

      do {
        last = curr
        curr = step(last)
        assert(last ⊑ curr)
      } while (curr != last)
      curr
    }
  }
}
