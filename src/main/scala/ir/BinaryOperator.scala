package me.viluon.tinyc
package ir

sealed trait BinaryOperator {
  import BinaryOperator._
  def eval(a: Int, b: Int): Option[Int] = this match {
    case Add() => Some(a + b)
    case Sub() => Some(a - b)
    case Mul() => Some(a * b)
    case Div() => if (b == 0) None else Some(a / b)
    case LessThan() => if (a < b) Some(1) else Some(0)
    case LessOrEqual() => if (a <= b) Some(1) else Some(0)
  }
}

object BinaryOperator {
  sealed trait Arithmetic extends BinaryOperator
  case class Add() extends Arithmetic
  case class Sub() extends Arithmetic
  case class Mul() extends Arithmetic
  case class Div() extends Arithmetic

  sealed trait Comparison extends BinaryOperator
  case class LessThan() extends Comparison
  case class LessOrEqual() extends Comparison
}
