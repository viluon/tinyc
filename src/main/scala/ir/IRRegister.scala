package me.viluon.tinyc
package ir

sealed trait IRRegister
object IRRegister {
  /**
   * A placeholder register of zero width.
   */
  case class NoReg() extends IRRegister
  case class IntReg(n: Int) extends IRRegister
}
