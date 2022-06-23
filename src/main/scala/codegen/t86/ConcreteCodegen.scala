package me.viluon.tinyc
package codegen.t86

import ir.IRNode._
import ir.{BinaryOperator, Continuation, IRRegister}

import cats.syntax.all._

object ConcreteCodegen {
  import tiny86._
  import tiny86.{Register => MachineReg}

  import AbstractCodegen._
  import CodegenOps._

  def translate(node: IRExpression[IRRegister]): Generator[MachineReg] = node match {
    case KInt(irReg, k) => for {
      reg <- lookupOrAlloc(irReg)
      _ <- addOne(new MOV(reg, k.toLong))
    } yield reg
    case BinOp(irReg, op, left, right) =>
      import BinaryOperator._
      for {
        l <- lookup(left)
        reg <- lookupOrAlloc(irReg)
        _ <- addOne(new MOV(reg, l))
        r <- lookup(right)
        _ <- op match {
          case Add() => addOne(new ADD(reg, r))
          case Sub() => addOne(new SUB(reg, r))
          case Mul() => addOne(new IMUL(reg, r))    // FIXME needs to follow type info
          case Div() => addOne(new IDIV(reg, r))    //  ditto
          case LessThan() => for {                  //  floats use FCMP and the registers have to be of equal type
            _ <- add(new CMP(l, r) :: getFlags(reg, ALUFlag.Sign))
          } yield ()
        }
      } yield reg
  }

  object ALUFlag {
    val Sign = 0x1
    val Zero = 0x2
    val Carry = 0x4
    val Overflow = 0x8
  }

  private def getFlags(reg: MachineReg, flag: Int): List[Instruction] = List(
    new MOV(reg, Register.Flags()),
    new AND(reg, flag)
  )

  def translateContinuation(cont: Cont): Generator[Unit] = cont match {
    case Continuation.Branch(condition, consequent, alternative) => ???
    case Continuation.Unconditional(next) => for {
      label <- addOne(new JMP(Label.empty().address()))
      // TODO arg list
      _ <- patch(label, next.callee)
    } yield ()
    case Continuation.Return(irReg) => for {
      reg <- lookup(irReg)
      // TODO there should be a way to get the next free register,
      //  although it's easier to rely on static assignment across block boundaries
      _ <- addOne(new MOV(new Register(0), reg))
      _ <- addOne(new RET())
    } yield ()
    case Continuation.Halt() => addOne(new HALT()).map(_ => ())
  }
}
