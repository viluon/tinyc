package me.viluon.tinyc
package codegen

import ir.IRProgram

trait Target {
  type Code

  def emit(ir: IRProgram[Nothing]): Code
}
