package me.viluon.tinyc

sealed trait Type
object Type {
  case object String extends Type
  case object Char extends Type
  case object Int extends Type
  case object Double extends Type
  case class Pointer(t: Type) extends Type
  case class Struct(fields: Map[String, Type]) extends Type
}
