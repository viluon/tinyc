package me.viluon.tinyc
package ir

sealed trait IRNode[Binding]

object IRNode {
  sealed trait IRType
  object IRType {
    case class IRInt() extends IRType
  }

  case class KInt[B](k: Int) extends IRNode[B]
  case class Block[B](
                       signature: List[IRType],
                       body: List[IRNode[B]],
                       cont: Continuation[B],
                       callingConvention: CallingConvention = CallingConvention.Unrestricted()
                     ) extends IRNode[B]
}
