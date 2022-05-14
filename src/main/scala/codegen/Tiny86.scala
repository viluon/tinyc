package me.viluon.tinyc
package codegen

import ir.IRNode.KInt
import ir.{IRNode, IRProgram}

import tiny86.{HALT, PUSH, Program, ProgramBuilder}

object Tiny86 extends Target {
  override type Code = Program

  override def emit(ir: IRProgram[Nothing]): Code = {
    val builder = new ProgramBuilder()
    ir.fns.foreach {
      case IRNode.Block(signature, body, cont, callingConvention) =>
        body.foreach {
          case KInt(k) => builder.add(new PUSH(k.toLong))
        }
    }

    builder.add(new HALT())
    builder.program()
  }
}
