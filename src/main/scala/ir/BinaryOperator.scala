package me.viluon.tinyc
package ir

sealed trait BinaryOperator
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
