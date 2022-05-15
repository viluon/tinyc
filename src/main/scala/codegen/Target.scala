package me.viluon.tinyc
package codegen

import ir.{IRNode, IRProgram, IRRegister}

trait Target {
  type Code

  def emit(ir: IRProgram[IRRegister]): Code
}
