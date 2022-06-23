package me.viluon.tinyc
package codegen.t86

import ir.IRNode._
import ir.{BinaryOperator, Continuation, IRRegister}

import cats.free.Free
import cats.syntax.all._

object AbstractCodegen {
  import tiny86._
  import tiny86.{Register => MachineReg}

  type Cont = Continuation[IRRegister, BasicBlockID]

  sealed trait CodegenOps[A]
  type Generator[A] = Free[CodegenOps, A]
  object CodegenOps {
    // ADT commands
    case class Add(instr: Instruction) extends CodegenOps[Label]
    case class Fresh() extends CodegenOps[MachineReg]
    case class Alloc(reg: IRRegister, mReg: MachineReg) extends CodegenOps[Unit]
    case class Lookup(reg: IRRegister) extends CodegenOps[Option[MachineReg]]
    case class Patch(label: Label, id: BasicBlockID) extends CodegenOps[Unit]
    // TODO this is effectively only needed by the interpreter (well, by whatever's running it).
    //  A modular (datatypes à la carte) free monad would allow for separation of concerns, where we mix this
    //  and possibly other commands in only where they're needed, while the interpreter supports the entire DSL.
    case class StartBlock(id: BasicBlockID) extends CodegenOps[Unit]

    // smart constructors
    def pure[A](x: A): Generator[A] = cats.Monad[Generator].pure(x)
    def add(instrs: List[Instruction]): Generator[List[Label]] = instrs.traverse(addOne)
    def addOne(instr: Instruction): Generator[Label] = Free.liftF(Add(instr))
    def fresh: Generator[MachineReg] = Free.liftF(Fresh())
    def alloc(reg: IRRegister, mReg: MachineReg): Generator[Unit] = Free.liftF(Alloc(reg, mReg))
    def safeLookup(register: IRRegister): Generator[Option[MachineReg]] = Free.liftF(Lookup(register))
    def lookup(reg: IRRegister): Generator[MachineReg] = safeLookup(reg).flatMap {
      case Some(mReg) => pure(mReg)
      case None => throw new IllegalStateException(s"register $reg not found")
    }
    def lookupOrAlloc(reg: IRRegister): Generator[MachineReg] = safeLookup(reg).flatMap {
      case Some(mReg) => pure(mReg)
      case None => for {
        mReg <- fresh
        _ <- alloc(reg, mReg)
      } yield mReg
    }
    def patch(label: Label, id: BasicBlockID): Generator[Unit] = Free.liftF(Patch(label, id))
    def startBlock(id: BasicBlockID): Generator[Unit] = Free.liftF(StartBlock(id))
  }
}
