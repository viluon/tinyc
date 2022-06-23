package me.viluon.tinyc
package codegen

import ir.{IRNode, IRProgram, IRRegister}
import IRNode.BasicBlockID

import cats.data.State
import cats.syntax.traverse.toTraverseOps
import cats.~>

object Tiny86 extends Target {
  import t86.AbstractCodegen.CodegenOps
  import t86.ConcreteCodegen.{translate, translateContinuation}
  import tiny86.{Register => MachineReg, _}

  override type Code = (Program, MachineReg)

  override def emit(program: IRProgram[IRRegister]): Code = gen(program)

  private def gen(program: IRProgram[IRRegister]): Code = {
    val (state, last) = program.traverse(fn =>
      for {
        _ <- CodegenOps.pure(())
        IRNode.Block(id, params, env, body, cont, callingConvention) = fn
        last <- body.traverse(translate)
        _ <- translateContinuation(cont)
      } yield id -> last.last
    ).map(_.toMap)
      .foldMap(programBuilder)
      .run(BuilderState())
      .value

    state.patched -> findOutputRegister(program, last)
  }

  // TODO DFS to find the first last register in a leaf block when starting from the entry block.
  def findOutputRegister(program: IRProgram[IRRegister], lastPerBlock: Map[BasicBlockID, MachineReg]): MachineReg = ???

  case class BuilderState(
                           builder: ProgramBuilder = new ProgramBuilder(),
                           patchTable: Map[Label, BasicBlockID] = Map(),
                           blockStart: Map[BasicBlockID, Label] = Map(),
                           openedBlocks: List[BasicBlockID] = List(),
                         ) {
    lazy val patched: Program = {
      patchTable.foreach {
        // TODO keep a buffer of freshly started blocks, when adding an instruction, flush the buffer,
        //  giving all the blocks the label of that instruction. Keep this information in a map, then just bridge here.
        //  Don't forget to flush the buffer when programBuilder is done.
        case (label, id) => builder.patch(label, ???)
      }
      builder.program()
    }
  }

  /**
   * Translate [[CodegenOps]] commands into operations on the [[ProgramBuilder]].
   * The [[ProgramBuilder]] is a mutable structure, so we thread a single instance
   * through the computation using a State monad.
   */
  def programBuilder: CodegenOps ~> State[BuilderState, *] = new (CodegenOps ~> State[BuilderState, *]) {
    private def modify[A](f: BuilderState => (BuilderState, A)): State[BuilderState, A] = for {
      s <- State.get[BuilderState]
      (s2, a) = f(s)
      _ <- State.set(s2)
    } yield a

    private def builder[A](f: ProgramBuilder => A): State[BuilderState, A] = State.get.map(f compose (_.builder))

    override def apply[A](fa: CodegenOps[A]): State[BuilderState, A] = fa match {
      case CodegenOps.Add(instr) => for {
        _ <- builder(_.add(instr))
      } yield ???
      case CodegenOps.Fresh() => ???
      case CodegenOps.Alloc(reg, mReg) => ???
      case CodegenOps.Lookup(reg) => ???
      case CodegenOps.Patch(label, id) => ???
    }
  }
}
