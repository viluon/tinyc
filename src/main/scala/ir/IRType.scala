package me.viluon.tinyc
package ir

sealed trait IRType
object IRType {
  def from(t: Type): IRType = t match {
    case Type.Int() => IRInt()
    case Type.Unit() => ???
    case Type.Char() => ???
    case Type.Double() => ???
    case Type.String() => ???
    case Type.Pointer(t) => ???
    case Type.Struct(fields) => ???
    case Type.Function(params, result) => ???
  }

  case class IRInt() extends IRType
}
