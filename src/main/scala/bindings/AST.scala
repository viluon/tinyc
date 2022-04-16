package me.viluon.tinyc
package bindings

import cats.derived._
import cats.kernel.Eq

sealed trait AST extends Product {
  def native: tinyc.AST
  def loc: tinyc.SourceLocation = native.location()
}

object AST {
  implicit val eqAST: Eq[AST] = semiauto.eq

  case class Integer(native: tinyc.ASTInteger) extends AST
  implicit val eqNativeASTInteger: Eq[tinyc.ASTInteger] = Eq.fromUniversalEquals
  implicit val eqInteger: Eq[Integer] = semiauto.eq

  case class Double(native: tinyc.ASTDouble) extends AST
  implicit val eqNativeASTDouble: Eq[tinyc.ASTDouble] = Eq.fromUniversalEquals
  implicit val eqDouble: Eq[Double] = semiauto.eq

  case class Char(native: tinyc.ASTChar) extends AST
  implicit val eqNativeASTChar: Eq[tinyc.ASTChar] = Eq.fromUniversalEquals
  implicit val eqChar: Eq[Char] = semiauto.eq

  case class String(native: tinyc.ASTString) extends AST
  implicit val eqNativeASTString: Eq[tinyc.ASTString] = Eq.fromUniversalEquals
  implicit val eqString: Eq[String] = semiauto.eq

  case class Identifier(native: tinyc.ASTIdentifier) extends AST {
    def symbol: tinyc.Symbol = native.getName
    def name: java.lang.String = symbol.name()
  }
  implicit val eqNativeASTIdentifier: Eq[tinyc.ASTIdentifier] = Eq.fromUniversalEquals
  implicit val eqIdentifier: Eq[Identifier] = semiauto.eq

  case class Type(native: tinyc.ASTType) extends AST
  implicit val eqNativeASTType: Eq[tinyc.ASTType] = Eq.fromUniversalEquals
  implicit val eqType: Eq[Type] = semiauto.eq

  case class PointerType(native: tinyc.ASTPointerType) extends AST
  implicit val eqNativeASTPointerType: Eq[tinyc.ASTPointerType] = Eq.fromUniversalEquals
  implicit val eqPointerType: Eq[PointerType] = semiauto.eq

  case class ArrayType(native: tinyc.ASTArrayType) extends AST
  implicit val eqNativeASTArrayType: Eq[tinyc.ASTArrayType] = Eq.fromUniversalEquals
  implicit val eqArrayType: Eq[ArrayType] = semiauto.eq

  case class NamedType(native: tinyc.ASTNamedType) extends AST
  implicit val eqNativeASTNamedType: Eq[tinyc.ASTNamedType] = Eq.fromUniversalEquals
  implicit val eqNamedType: Eq[NamedType] = semiauto.eq

  case class Sequence(native: tinyc.ASTSequence) extends AST
  implicit val eqNativeASTSequence: Eq[tinyc.ASTSequence] = Eq.fromUniversalEquals
  implicit val eqSequence: Eq[Sequence] = semiauto.eq

  case class Block(native: tinyc.ASTBlock) extends AST {
    val stmts: List[AST] = native.getBody.toArray.asInstanceOf[Array[AST]].toList
  }
  implicit val eqNativeASTBlock: Eq[tinyc.ASTBlock] = Eq.fromUniversalEquals
  implicit val eqBlock: Eq[Block] = semiauto.eq

  case class VarDecl(native: tinyc.ASTVarDecl) extends AST
  implicit val eqNativeASTVarDecl: Eq[tinyc.ASTVarDecl] = Eq.fromUniversalEquals
  implicit val eqVarDecl: Eq[VarDecl] = semiauto.eq

  case class FunDecl(native: tinyc.ASTFunDecl) extends AST
  implicit val eqNativeASTFunDecl: Eq[tinyc.ASTFunDecl] = Eq.fromUniversalEquals
  implicit val eqFunDecl: Eq[FunDecl] = semiauto.eq

  case class StructDecl(native: tinyc.ASTStructDecl) extends AST
  implicit val eqNativeASTStructDecl: Eq[tinyc.ASTStructDecl] = Eq.fromUniversalEquals
  implicit val eqStructDecl: Eq[StructDecl] = semiauto.eq

  case class FunPtrDecl(native: tinyc.ASTFunPtrDecl) extends AST
  implicit val eqNativeASTFunPtrDecl: Eq[tinyc.ASTFunPtrDecl] = Eq.fromUniversalEquals
  implicit val eqFunPtrDecl: Eq[FunPtrDecl] = semiauto.eq

  case class If(native: tinyc.ASTIf) extends AST
  implicit val eqNativeASTIf: Eq[tinyc.ASTIf] = Eq.fromUniversalEquals
  implicit val eqIf: Eq[If] = semiauto.eq

  case class Switch(native: tinyc.ASTSwitch) extends AST
  implicit val eqNativeASTSwitch: Eq[tinyc.ASTSwitch] = Eq.fromUniversalEquals
  implicit val eqSwitch: Eq[Switch] = semiauto.eq

  case class While(native: tinyc.ASTWhile) extends AST
  implicit val eqNativeASTWhile: Eq[tinyc.ASTWhile] = Eq.fromUniversalEquals
  implicit val eqWhile: Eq[While] = semiauto.eq

  case class DoWhile(native: tinyc.ASTDoWhile) extends AST
  implicit val eqNativeASTDoWhile: Eq[tinyc.ASTDoWhile] = Eq.fromUniversalEquals
  implicit val eqDoWhile: Eq[DoWhile] = semiauto.eq

  case class For(native: tinyc.ASTFor) extends AST
  implicit val eqNativeASTFor: Eq[tinyc.ASTFor] = Eq.fromUniversalEquals
  implicit val eqFor: Eq[For] = semiauto.eq

  case class Break(native: tinyc.ASTBreak) extends AST
  implicit val eqNativeASTBreak: Eq[tinyc.ASTBreak] = Eq.fromUniversalEquals
  implicit val eqBreak: Eq[Break] = semiauto.eq

  case class Continue(native: tinyc.ASTContinue) extends AST
  implicit val eqNativeASTContinue: Eq[tinyc.ASTContinue] = Eq.fromUniversalEquals
  implicit val eqContinue: Eq[Continue] = semiauto.eq

  case class Return(native: tinyc.ASTReturn) extends AST
  implicit val eqNativeASTReturn: Eq[tinyc.ASTReturn] = Eq.fromUniversalEquals
  implicit val eqReturn: Eq[Return] = semiauto.eq

  case class BinaryOp(native: tinyc.ASTBinaryOp) extends AST
  implicit val eqNativeASTBinaryOp: Eq[tinyc.ASTBinaryOp] = Eq.fromUniversalEquals
  implicit val eqBinaryOp: Eq[BinaryOp] = semiauto.eq

  case class Assignment(native: tinyc.ASTAssignment) extends AST
  implicit val eqNativeASTAssignment: Eq[tinyc.ASTAssignment] = Eq.fromUniversalEquals
  implicit val eqAssignment: Eq[Assignment] = semiauto.eq

  case class UnaryOp(native: tinyc.ASTUnaryOp) extends AST
  implicit val eqNativeASTUnaryOp: Eq[tinyc.ASTUnaryOp] = Eq.fromUniversalEquals
  implicit val eqUnaryOp: Eq[UnaryOp] = semiauto.eq

  case class UnaryPostOp(native: tinyc.ASTUnaryPostOp) extends AST
  implicit val eqNativeASTUnaryPostOp: Eq[tinyc.ASTUnaryPostOp] = Eq.fromUniversalEquals
  implicit val eqUnaryPostOp: Eq[UnaryPostOp] = semiauto.eq

  case class Address(native: tinyc.ASTAddress) extends AST
  implicit val eqNativeASTAddress: Eq[tinyc.ASTAddress] = Eq.fromUniversalEquals
  implicit val eqAddress: Eq[Address] = semiauto.eq

  case class Deref(native: tinyc.ASTDeref) extends AST
  implicit val eqNativeASTDeref: Eq[tinyc.ASTDeref] = Eq.fromUniversalEquals
  implicit val eqDeref: Eq[Deref] = semiauto.eq

  case class Index(native: tinyc.ASTIndex) extends AST
  implicit val eqNativeASTIndex: Eq[tinyc.ASTIndex] = Eq.fromUniversalEquals
  implicit val eqIndex: Eq[Index] = semiauto.eq

  case class Member(native: tinyc.ASTMember) extends AST
  implicit val eqNativeASTMember: Eq[tinyc.ASTMember] = Eq.fromUniversalEquals
  implicit val eqMember: Eq[Member] = semiauto.eq

  case class MemberPtr(native: tinyc.ASTMemberPtr) extends AST
  implicit val eqNativeASTMemberPtr: Eq[tinyc.ASTMemberPtr] = Eq.fromUniversalEquals
  implicit val eqMemberPtr: Eq[MemberPtr] = semiauto.eq

  case class Call(native: tinyc.ASTCall) extends AST
  implicit val eqNativeASTCall: Eq[tinyc.ASTCall] = Eq.fromUniversalEquals
  implicit val eqCall: Eq[Call] = semiauto.eq

  case class Cast(native: tinyc.ASTCast) extends AST
  implicit val eqNativeASTCast: Eq[tinyc.ASTCast] = Eq.fromUniversalEquals
  implicit val eqCast: Eq[Cast] = semiauto.eq

  case class Write(native: tinyc.ASTWrite) extends AST
  implicit val eqNativeASTWrite: Eq[tinyc.ASTWrite] = Eq.fromUniversalEquals
  implicit val eqWrite: Eq[Write] = semiauto.eq

  case class Read(native: tinyc.ASTRead) extends AST
  implicit val eqNativeASTRead: Eq[tinyc.ASTRead] = Eq.fromUniversalEquals
  implicit val eqRead: Eq[Read] = semiauto.eq
}
