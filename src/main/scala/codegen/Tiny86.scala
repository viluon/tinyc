package me.viluon.tinyc
package codegen

import ir.{GraphTraversal, IRNode, IRProgram, IRRegister}

import IRNode.BasicBlockID
import cats.data.State
import cats.syntax.traverse.toTraverseOps
import cats.{Id, ~>}

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
        _ <- CodegenOps.startBlock(id)
        last <- body.traverse(translate)
        _ <- translateContinuation(cont)
      } yield id -> last
    ).map(_.toMap)
      .foldMap(programBuilder)
      .run(BuilderState())
      .value

    state.patched -> findOutputRegister(program, last.filter(_._2.nonEmpty).flatMap(p => p._2.map(p._1 -> _)))
  }

  /**
   * DFS to find the first last register in a leaf block when starting from the entry block.
   */
  def findOutputRegister(program: IRProgram[IRRegister], lastPerBlock: Map[BasicBlockID, MachineReg]): MachineReg =
    new GraphTraversal[Id, BasicBlockID, MachineReg] {
      override def successors(node: BasicBlockID): Id[Set[BasicBlockID]] = program.blocks(node).cont.callees.toSet
      override def visit(node: BasicBlockID): Id[Option[MachineReg]] = None
      override def leaf(node: BasicBlockID): Id[Option[MachineReg]] = lastPerBlock.get(node)
    }.dfs(program.entry)._2.get

  case class BuilderState(
                           builder: ProgramBuilder = new ProgramBuilder(),
                           patchTable: Map[Label, BasicBlockID] = Map(),
                           blockStart: Map[BasicBlockID, Label] = Map(),
                           openedBlocks: List[BasicBlockID] = List(),
                           allocatedRegisters: Map[IRRegister, MachineReg] = Map(),
                           regCtr: Int = 0,
                         ) {
    lazy val patched: Program = {
      patchTable.foreach {
        case (label, id) =>
          println(s"patching $label -> $id (${blockStart(id)})")
          builder.patch(label, blockStart(id))
      }
      builder.program()
    }
  }

  /**
   * Translate [[CodegenOps]] commands into operations on the [[ProgramBuilder]].
   * The [[ProgramBuilder]] is a mutable structure, so we thread a single instance
   * of it through the computation using a State monad.
   */
  def programBuilder: CodegenOps ~> State[BuilderState, *] = new (CodegenOps ~> State[BuilderState, *]) {
    private def modify[A](f: BuilderState => (BuilderState, A)): State[BuilderState, A] = for {
      s <- State.get[BuilderState]
      (s2, a) = f(s)
      _ <- State.set(s2)
    } yield a

    private def builder[A](f: ProgramBuilder => A): State[BuilderState, A] = State.get.map(f compose (_.builder))

    override def apply[A](fa: CodegenOps[A]): State[BuilderState, A] = {
      println(fa)
      fa match {
        case CodegenOps.Add(instr) => for {
          label <- builder(_.add(instr))
          // set the start label of all opened blocks to the just added instruction
          _ <- modify(state => (state.openedBlocks match {
            case Nil => state
            case blocks => state.copy(
              openedBlocks = Nil,
              blockStart = state.blockStart ++ (blocks.map(_ -> label))
            )
          }) -> ())
        } yield label
        case CodegenOps.StartBlock(id) =>
          modify(s => s.copy(openedBlocks = id :: s.openedBlocks) -> ())
        case CodegenOps.Fresh() =>
          modify(s => s.copy(regCtr = s.regCtr + 1) -> new MachineReg(s.regCtr))
        case CodegenOps.Alloc(reg, mReg) =>
          modify(s => s.copy(allocatedRegisters = s.allocatedRegisters + (reg -> mReg)) -> ())
        case CodegenOps.Lookup(reg) =>
          State.get.map(_.allocatedRegisters.get(reg))
        case CodegenOps.Patch(label, id) =>
          modify(s => s.copy(patchTable = s.patchTable + (label -> id)) -> ())
      }
    }
  }
}
