package me.viluon.tinyc
package ir

sealed trait BinaryOperator
object BinaryOperator {
  case class Add() extends BinaryOperator
  case class Sub() extends BinaryOperator
  case class Mul() extends BinaryOperator
  case class Div() extends BinaryOperator
}
