package me.viluon.tinyc
package ir

/**
 * A continuation, the terminator of a basic block.
 * @tparam B The binder type
 * @tparam L The label type
 */
sealed trait Continuation[B, L]

object Continuation {
  case class Branch[B, L](condition: B, consequent: L, alternative: L)
    extends Continuation[B, L]
  case class Unconditional[B, L](next: L) extends Continuation[B, L]
  case class Halt[B, L]() extends Continuation[B, L]
}
