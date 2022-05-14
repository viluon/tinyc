package me.viluon.tinyc
package ir

import bindings.AST

import cats.implicits._

object Lowering {
  def lowerTopLevel(ast: AST): Either[String, IRProgram[Nothing]] = ast match {
    case b: AST.Block => b.stmts.traverse(lower).map(_.map(_.asInstanceOf[IRNode.Block[Nothing]])).map(IRProgram[Nothing])
    case _ => Left(s"top level must be a block, which $ast certainly isn't")
  }

  private def lower(ast: AST): Either[String, IRNode[Nothing]] = ast match {
    case int: AST.Integer => Right(IRNode.KInt(int.v))
    case _: AST.Double => ???
    case _: AST.Char => ???
    case _: AST.String => ???
    case _: AST.Identifier => ???
    case _: AST.Type => ???
    case _: AST.PointerType => ???
    case _: AST.ArrayType => ???
    case _: AST.NamedType => ???
    case _: AST.Sequence => ???
    case b: AST.Block => b.stmts.traverse(lower).map(IRNode.Block[Nothing](
      Nil /*TODO: let's refactor lower() into a Writer that keeps track of dependencies*/ ,
      _,
      Continuation.Halt()
    ))
    case _: AST.VarDecl => ???
    case fn: AST.FunDecl => lower(fn.body) // TODO
    case _: AST.StructDecl => ???
    case _: AST.FunPtrDecl => ???
    case _: AST.If => ???
    case _: AST.Switch => ???
    case _: AST.While => ???
    case _: AST.DoWhile => ???
    case _: AST.For => ???
    case _: AST.Break => ???
    case _: AST.Continue => ???
    case ret: AST.Return => lower(ret.value) // TODO
    case _: AST.BinaryOp => ???
    case _: AST.Assignment => ???
    case _: AST.UnaryOp => ???
    case _: AST.UnaryPostOp => ???
    case _: AST.Address => ???
    case _: AST.Deref => ???
    case _: AST.Index => ???
    case _: AST.Member => ???
    case _: AST.MemberPtr => ???
    case _: AST.Call => ???
    case _: AST.Cast => ???
    case _: AST.Write => ???
    case _: AST.Read => ???
  }
}
