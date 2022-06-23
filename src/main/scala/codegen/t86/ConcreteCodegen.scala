package me.viluon.tinyc
package codegen.t86

import ir.IRNode._
import ir.{BinaryOperator, Continuation, IRRegister}

import cats.syntax.all._

object ConcreteCodegen {
  import tiny86.{Register => MachineReg, _}

  import AbstractCodegen._
  import CodegenOps._

  private def emitComparison(target: MachineReg, l: MachineReg, r: MachineReg, instr: Instruction): Generator[Unit] =
    for {
      // FIXME floats use FCMP and the registers have to be of equal type
      _ <- emit(new CMP(l, r))
      br <- emit(instr)
      // alternative
      _ <- emit(new MOV(target, 0))
      altEnd <- emit(new JMP(NoLabel))
      // consequent
      cons <- emit(new MOV(target, 1))
      end <- emit(new NOP())
      _ <- rawPatch(br, cons)
      _ <- rawPatch(altEnd, end)
    } yield ()

  def translate(node: IRExpression[IRRegister]): Generator[MachineReg] = node match {
    case KInt(irReg, k) => for {
      reg <- lookupOrAlloc(irReg)
      _ <- emit(new MOV(reg, k.toLong))
    } yield reg
    case BinOp(irReg, op, left, right) =>
      import BinaryOperator._
      for {
        l <- lookup(left)
        reg <- lookupOrAlloc(irReg)
        r <- lookup(right)
        _ <- op match {
          case op: Arithmetic => for {
            _ <- emit(new MOV(reg, l))
            _ <- emit(op match {
              case Add() => new ADD(reg, r)
              case Sub() => new SUB(reg, r)
              case Mul() => new IMUL(reg, r)    // FIXME needs to follow type info
              case Div() => new IDIV(reg, r)    //  ditto
            })
          } yield reg
          case op: Comparison => for {
            _ <- emitComparison(reg, l, r, op match {
              case LessThan() => new JL(NoLabel)
              case LessOrEqual() => new JLE(NoLabel)
            })
          } yield reg
        }
      } yield reg
    case Call(irReg, callee, args) => for {
      reg <- lookupOrAlloc(irReg)
      _ <- args.traverse_(arg => for {
        argReg <- lookup(arg)
        _ <- emit(new PUSH(argReg))
      } yield ())
      label <- emit(new CALL(NoLabel))
      _ <- patch(label, callee)
    } yield reg
  }

  object ALUFlag {
    val Sign = 0x1
    val Zero = 0x2
    val Carry = 0x4
    val Overflow = 0x8
  }

  def forwardArgs(target: Continuation.Target[IRRegister, BasicBlockID]): Generator[Unit] = for {
    block <- getBlock(target.callee)
    params = block.paramRegs
    allocatedParams <- params.traverse(lookupOrAlloc)
    _ <- target.args.zip(allocatedParams).traverse_ { case (arg, param) =>
      lookup(arg).flatMap(argReg =>
        if (param != argReg)
          emit(new MOV(param, argReg)).map(_ => ())
        else pure(())
      )
    }
  } yield ()

  def translateContinuation(cont: Cont): Generator[Unit] = cont match {
    case Continuation.Branch(condition, consequent, alternative) => for {
      cond <- lookup(condition)
      _ <- emit(new CMP(cond, 0))
      // FIXME the alternative arg setup should be moved into a new raw block
      //  that ends with a jump to alternative.callee
      _ <- forwardArgs(alternative)
      _ <- emitJump(new JE(_), alternative.callee)
      _ <- forwardArgs(consequent)
      _ <- emitJump(new JMP(_), consequent.callee)
    } yield ()
    case Continuation.Unconditional(next) => for {
      _ <- forwardArgs(next)
      _ <- emitJump(new JMP(_), next.callee)
    } yield ()
    case Continuation.Return(irReg) => for {
      reg <- lookup(irReg)
      // TODO there should be a way to get the next free register,
      //  although it's easier to rely on static assignment across block boundaries
      _ <- emit(new MOV(new MachineReg(0), reg))
      _ <- emit(new RET())
    } yield ()
    case Continuation.Halt() => emit(new HALT()).map(_ => ())
  }
}
