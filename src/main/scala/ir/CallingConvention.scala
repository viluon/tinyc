package me.viluon.tinyc
package ir

sealed trait CallingConvention
object CallingConvention {
  case class Unrestricted() extends CallingConvention
}
