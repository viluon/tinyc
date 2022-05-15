package me.viluon.tinyc
package ir

sealed trait CallingConvention
object CallingConvention {
  case class Function() extends CallingConvention
  case class Unrestricted() extends CallingConvention
}
