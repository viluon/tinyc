package me.viluon.tinyc
package codegen

import ir.IRNode.KInt
import ir.{IRNode, IRProgram}

import tiny86.{HALT, MOV, MemoryImmediate, PUSH, Program, ProgramBuilder, Register}

import java.math.BigInteger

object Tiny86 extends Target {
  override type Code = Program

  override def emit(ir: IRProgram[Nothing]): Code = {
    val builder = new ProgramBuilder()
    ir.fns.foreach {
      case IRNode.Block(signature, body, cont, callingConvention) =>
        body.foreach {
          case KInt(k) => builder.add(new MOV(new Register(0), k.toLong))
        }
    }

    builder.add(new HALT())
    builder.program()
  }
}
