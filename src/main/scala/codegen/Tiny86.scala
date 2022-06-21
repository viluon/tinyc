package me.viluon.tinyc
package codegen

import ir.IRNode.{BinOp, Block, IRExpression, KInt}
import ir.{BinaryOperator, IRNode, IRProgram, IRRegister}

import cats.data.{StateT, Writer}
import cats.syntax.traverse.toTraverseOps

object Tiny86 extends Target {
  import tiny86._
  import tiny86.{Register => MachineReg}
  override type Code = (Program, MachineReg)

  private type GenLog = List[Instruction]
  private case class GenState(regCtr: Int = 0, allocation: Map[IRRegister, MachineReg] = Map())
  private type Generator[A] = StateT[Writer[GenLog, *], GenState, A]

  override def emit(program: IRProgram[IRRegister]): Code = gen(program)

  private def get: Generator[GenState] = StateT.get
  private def put(n: GenState): Generator[Unit] = StateT.set(n)
  private def modify(f: GenState => GenState): Generator[Unit] = StateT.modify(f)
  private def pure[A](x: A): Generator[A] = cats.Monad[Generator].pure(x)
  private def add(instr: Instruction): Generator[Unit] = StateT.liftF(Writer.tell(List(instr)))
  private def fresh: Generator[MachineReg] = for {
    s <- get
    reg = new Register(s.regCtr)
    _ <- put(s.copy(regCtr = s.regCtr + 1))
  } yield reg
  private def alloc(reg: IRRegister, mReg: MachineReg): Generator[Unit] =
    modify { s => s.copy(allocation = s.allocation.updated(reg, mReg))}
  private def lookup(register: IRRegister): Generator[MachineReg] = get.map(_.allocation(register))
  private def lookupOrAlloc(reg: IRRegister): Generator[MachineReg] = get.map(_.allocation.get(reg)).flatMap {
    case Some(mReg) => pure(mReg)
    case None => for {
      mReg <- fresh
      _ <- alloc(reg, mReg)
    } yield mReg
  }

  private def gen(program: IRProgram[IRRegister]): Code = {
    val (code, (_, last)) = program.traverse(fn => for {
      _ <- pure(())
      IRNode.Block(signature, body, cont, callingConvention) = fn
      last <- body.traverse(translate)
    } yield last).run(GenState()).run

    val builder = new ProgramBuilder()
    (code :+ new HALT()).foreach(builder.add)
    builder.program() -> last.flatten.last
  }

  private def translate(node: IRExpression[IRRegister]): Generator[MachineReg] = node match {
    case KInt(irReg, k) => for {
      reg <- lookupOrAlloc(irReg)
      _ <- add(new MOV(reg, k.toLong))
    } yield reg
    case BinOp(irReg, op, left, right) =>
      import BinaryOperator._
      for {
        l <- lookup(left)
        reg <- lookupOrAlloc(irReg)
        _ <- add(new MOV(reg, l))
        r <- lookup(right)
        _ <- add(op match {
          case Add() => new ADD(reg, r)
          case Sub() => new SUB(reg, r)
          case Mul() => new IMUL(reg, r) // FIXME needs to follow type info
          case Div() => new IDIV(reg, r) //  ditto
        })
      } yield reg
  }
}
