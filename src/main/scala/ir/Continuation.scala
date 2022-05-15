package me.viluon.tinyc
package ir

sealed trait Continuation[B]
object Continuation {
  case class Unconditional[B](next: IRNode.Block[B]) extends Continuation[B]
  case class Halt[B]() extends Continuation[B]
}
