package me.viluon.tinyc

import cats.Show
import cats.derived._
import cats.syntax.show._
import cats.kernel.Eq

sealed trait Type
object Type {
  implicit val eqType: Eq[Type] = semiauto.eq
  implicit val showType: Show[Type] = Show.show {
    case Int() => "int"
    case Unit() => "void"
    case Char() => "char"
    case Double() => "double"
    case String() => "string"
    case Pointer(t) => "*" + t.show
    case Struct(fields) => fields.toList.map(p => p._1 + ": " + p._2.show).sorted.mkString("{", ", ", "}")
    case Function(params, result) =>
      params.map(p => p._1 + ": " + p._2.show).mkString("(", ", ", ")") + " -> " + result.show
  }

  case class Int() extends Type
  implicit val eqInt: Eq[Int] = semiauto.eq

  case class Unit() extends Type
  implicit val eqUnit: Eq[Unit] = semiauto.eq

  case class Char() extends Type
  implicit val eqChar: Eq[Char] = semiauto.eq

  case class Double() extends Type
  implicit val eqDouble: Eq[Double] = semiauto.eq

  case class String() extends Type
  implicit val eqString: Eq[String] = semiauto.eq

  case class Pointer(t: Type) extends Type
  implicit val eqPointer: Eq[Pointer] = semiauto.eq

  case class Struct(fields: Map[String, Type]) extends Type
  implicit val eqStruct: Eq[Struct] = semiauto.eq

  case class Function(params: List[(String, Type)], result: Type) extends Type
  implicit val eqFunction: Eq[Function] = semiauto.eq
}
