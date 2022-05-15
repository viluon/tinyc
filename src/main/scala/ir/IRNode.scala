package me.viluon.tinyc
package ir

sealed trait IRNode[Binding]

object IRNode {
  case class Block[B](
                       signature: List[IRType],
                       body: List[IRExpression[B]],
                       cont: Continuation[B],
                       callingConvention: CallingConvention = CallingConvention.Unrestricted()
                     ) extends IRNode[B]

  sealed trait IRExpression[B] extends IRNode[B]

  case class KInt[B](k: Int) extends IRExpression[B]
  case class BinOp[B](op: BinaryOperator, l: B, r: B) extends IRExpression[B]
  case class Load[B](target: B) extends IRExpression[B]
}
