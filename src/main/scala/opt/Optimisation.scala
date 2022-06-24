package me.viluon.tinyc
package opt

import ir.{IRProgram, IRRegister}

import scala.util.Try

object Optimisation {
  def optimise(ir: IRProgram[IRRegister]): Try[IRProgram[IRRegister]] = {
    Try(ir)
  }
}
