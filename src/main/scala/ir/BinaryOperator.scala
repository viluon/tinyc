package me.viluon.tinyc
package ir

sealed trait BinaryOperator
object BinaryOperator {
  case class Add() extends BinaryOperator
  case class Sub() extends BinaryOperator
  case class Mul() extends BinaryOperator
  case class Div() extends BinaryOperator

  sealed trait Comparison extends BinaryOperator
  case class LessThan() extends Comparison
  case class LessOrEqual() extends Comparison
}
