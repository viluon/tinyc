package me.viluon.tinyc
package ir

sealed trait CallingConvention
object CallingConvention {
  case class None() extends CallingConvention
}
