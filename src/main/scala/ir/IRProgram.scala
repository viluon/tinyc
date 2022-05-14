package me.viluon.tinyc
package ir

case class IRProgram[B](fns: List[IRNode.Block[B]])
