package me.viluon.tinyc
package codegen

import ir.{IRProgram, IRRegister}

trait Target {
  type Code

  def emit(ir: IRProgram[IRRegister]): Code
}
