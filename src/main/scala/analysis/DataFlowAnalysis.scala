package me.viluon.tinyc
package analysis

import ir.IRNode.{BasicBlockID, IRExpression}
import ir.{IRNode, IRProgram, IRRegister}

trait DataFlowAnalysis {
  type AbstractValue
  type AbstractEnv = Map[IRRegister, AbstractValue]
  // we simply index the body of the block
  type BlockState = Map[Node, AbstractEnv]
  type ProgramState = Map[BasicBlockID, BlockState]
  type Expr = IRExpression[IRRegister]
  type Node = (Expr, Int)

  def vLat: Lattice[AbstractValue]
  def nodeLat(node: Node): Lattice[AbstractEnv]
  def blockLat(fn: BasicBlockID): Lattice[BlockState]
  def programLat: Lattice[ProgramState]
  def forward: Boolean
  def must: Boolean
  def transfer(node: Node, state: AbstractEnv): AbstractEnv
}

object DataFlowAnalysis {
  abstract class Builder[E](lat: Lattice[E],
                            _forward: Boolean = true,
                            _must: Boolean = true
                           )(_ir: IRProgram[IRRegister])
    extends DataFlowAnalysis with FixpointComputation {
    import scala.language.implicitConversions

    implicit val vLat: Lattice[AbstractValue] = if (must) lat else Lattice.invLat(lat)

    override type AbstractValue = E
    private lazy val ir: IRProgram[IRRegister] = if (forward) _ir else ???

    override implicit def nodeLat(node: Node): Lattice[AbstractEnv] = Lattice.mapLat(???, _ => vLat)
    override implicit def blockLat(fn: BasicBlockID): Lattice[BlockState] = Lattice.mapLat(ir.blocks(fn).body.zipWithIndex, nodeLat)
    override implicit lazy val programLat: Lattice[ProgramState] = Lattice.mapLat(ir.blocks.keys, blockLat)
    override def forward: Boolean = _forward
    override def must: Boolean = _must

    def fixpoint(): ProgramState = {
      // FIXME addition of parameters should happen elsewhere
      def addParamsToScope(node: Node, blockID: BasicBlockID, env: AbstractEnv) = {
        // bind each param to ⊤
        (node, env ++ ir.blocks(blockID).paramRegs.map(_ -> vLat.top))
      }

      fixpoint(ir, programLat.bot.map {
        case (blockID, blockState) => blockID -> blockState.map {
          case (node, env) => addParamsToScope(node, blockID, env)
          case (node, env) => addParamsToScope(node, blockID, env)
          case x => x
        }
      })
    }
  }
}
