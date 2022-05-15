package me.viluon.tinyc
package ir

sealed trait IRType
object IRType {
  case class IRInt() extends IRType
}
