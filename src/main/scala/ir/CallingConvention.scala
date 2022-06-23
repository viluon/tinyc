package me.viluon.tinyc
package ir

sealed trait CallingConvention
object CallingConvention {
  /**
   * The function calling convention relies on stack manipulation and will always return in IR leaves.
   * Therefore, jumping to the function's address is incorrect, since the return instruction pops the stack.
   */
  case class Function() extends CallingConvention
  case class Unrestricted() extends CallingConvention
}
