package me.viluon.tinyc
package ir

sealed trait Continuation[B]
object Continuation {
  case class Halt[B]() extends Continuation[B]
}
