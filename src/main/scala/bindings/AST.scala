package me.viluon.tinyc
package bindings

import cats.derived._
import cats.kernel.Eq

import java.util
import java.util.RandomAccess

sealed trait AST {
  def foreign: tinyc.AST
  Option(foreign).get
  def loc: tinyc.SourceLocation = Option(foreign.location()).get
}

object AST {
  import ASTBridge._

  private def unvector[A](vec: util.AbstractList[A] with RandomAccess): List[A] = {
    if (vec == null) throw new NullPointerException()
    vec.toArray.toList.map(_.asInstanceOf[A])
  }

  implicit val eqAST: Eq[AST] = semiauto.eq

  case class Integer(foreign: tinyc.ASTInteger) extends AST {
    def v: Int = foreign.getValue.toInt
  }
  implicit val eqForeignASTInteger: Eq[tinyc.ASTInteger] = Eq.fromUniversalEquals
  implicit val eqInteger: Eq[Integer] = semiauto.eq

  case class Double(foreign: tinyc.ASTDouble) extends AST
  implicit val eqForeignASTDouble: Eq[tinyc.ASTDouble] = Eq.fromUniversalEquals
  implicit val eqDouble: Eq[Double] = semiauto.eq

  case class Char(foreign: tinyc.ASTChar) extends AST
  implicit val eqForeignASTChar: Eq[tinyc.ASTChar] = Eq.fromUniversalEquals
  implicit val eqChar: Eq[Char] = semiauto.eq

  case class String(foreign: tinyc.ASTString) extends AST
  implicit val eqForeignASTString: Eq[tinyc.ASTString] = Eq.fromUniversalEquals
  implicit val eqString: Eq[String] = semiauto.eq

  case class Identifier(foreign: tinyc.ASTIdentifier) extends AST {
    def symbol: tinyc.Symbol = Option(foreign.getName).get
    def name: java.lang.String = Option(symbol.name()).get
  }
  implicit val eqForeignASTIdentifier: Eq[tinyc.ASTIdentifier] = Eq.fromUniversalEquals
  implicit val eqIdentifier: Eq[Identifier] = semiauto.eq

  case class Type(foreign: tinyc.ASTType) extends AST
  implicit val eqForeignASTType: Eq[tinyc.ASTType] = Eq.fromUniversalEquals
  implicit val eqType: Eq[Type] = semiauto.eq

  case class PointerType(foreign: tinyc.ASTPointerType) extends AST
  implicit val eqForeignASTPointerType: Eq[tinyc.ASTPointerType] = Eq.fromUniversalEquals
  implicit val eqPointerType: Eq[PointerType] = semiauto.eq

  case class ArrayType(foreign: tinyc.ASTArrayType) extends AST
  implicit val eqForeignASTArrayType: Eq[tinyc.ASTArrayType] = Eq.fromUniversalEquals
  implicit val eqArrayType: Eq[ArrayType] = semiauto.eq

  case class NamedType(foreign: tinyc.ASTNamedType) extends AST
  implicit val eqForeignASTNamedType: Eq[tinyc.ASTNamedType] = Eq.fromUniversalEquals
  implicit val eqNamedType: Eq[NamedType] = semiauto.eq

  case class Sequence(foreign: tinyc.ASTSequence) extends AST {
    lazy val body: List[AST] = Option(unvector(foreign.getBody).map(_.wrapped)).get
  }
  implicit val eqForeignASTSequence: Eq[tinyc.ASTSequence] = Eq.fromUniversalEquals
  implicit val eqSequence: Eq[Sequence] = semiauto.eq

  case class Block(foreign: tinyc.ASTBlock) extends AST {
    lazy val stmts: List[AST] = Option(Option(Option(foreign.getBody).get.toArray).get.toList).get.map(_.asInstanceOf[tinyc.AST].wrapped)
  }
  implicit val eqForeignASTBlock: Eq[tinyc.ASTBlock] = Eq.fromUniversalEquals
  implicit val eqBlock: Eq[Block] = Eq.fromUniversalEquals

  case class VarDecl(foreign: tinyc.ASTVarDecl) extends AST {
    lazy val value: AST = Option(foreign.getValue).get.wrapped
    lazy val typ: me.viluon.tinyc.Type = Option(foreign.getVarType.nativeType).get
    lazy val ident: AST.Identifier = Option(foreign.getName.wrapped.asInstanceOf[Identifier]).get
  }
  implicit val eqForeignASTVarDecl: Eq[tinyc.ASTVarDecl] = Eq.fromUniversalEquals
  implicit val eqVarDecl: Eq[VarDecl] = semiauto.eq

  case class FunDecl(foreign: tinyc.ASTFunDecl) extends AST {
    lazy val body: AST.Block = foreign.getBody.wrapped.asInstanceOf[AST.Block]
    lazy val params: List[(tinyc.Symbol, me.viluon.tinyc.Type)] =
      unvector(foreign.getArgs).map(p => p.getSecond.getName -> p.getFirst.nativeType)
    lazy val returnType: me.viluon.tinyc.Type = foreign.getTypeDecl.nativeType
  }
  implicit val eqForeignASTFunDecl: Eq[tinyc.ASTFunDecl] = Eq.fromUniversalEquals
  implicit val eqFunDecl: Eq[FunDecl] = semiauto.eq

  case class StructDecl(foreign: tinyc.ASTStructDecl) extends AST
  implicit val eqForeignASTStructDecl: Eq[tinyc.ASTStructDecl] = Eq.fromUniversalEquals
  implicit val eqStructDecl: Eq[StructDecl] = semiauto.eq

  case class FunPtrDecl(foreign: tinyc.ASTFunPtrDecl) extends AST
  implicit val eqForeignASTFunPtrDecl: Eq[tinyc.ASTFunPtrDecl] = Eq.fromUniversalEquals
  implicit val eqFunPtrDecl: Eq[FunPtrDecl] = semiauto.eq

  case class If(foreign: tinyc.ASTIf) extends AST {
    lazy val condition: AST = foreign.getCond.wrapped
    lazy val consequent: AST = foreign.getTrueCase.wrapped
    lazy val alternative: Option[AST] = Option(foreign.getFalseCase).map(_.wrapped)
  }
  implicit val eqForeignASTIf: Eq[tinyc.ASTIf] = Eq.fromUniversalEquals
  implicit val eqIf: Eq[If] = semiauto.eq

  case class Switch(foreign: tinyc.ASTSwitch) extends AST
  implicit val eqForeignASTSwitch: Eq[tinyc.ASTSwitch] = Eq.fromUniversalEquals
  implicit val eqSwitch: Eq[Switch] = semiauto.eq

  case class While(foreign: tinyc.ASTWhile) extends AST
  implicit val eqForeignASTWhile: Eq[tinyc.ASTWhile] = Eq.fromUniversalEquals
  implicit val eqWhile: Eq[While] = semiauto.eq

  case class DoWhile(foreign: tinyc.ASTDoWhile) extends AST
  implicit val eqForeignASTDoWhile: Eq[tinyc.ASTDoWhile] = Eq.fromUniversalEquals
  implicit val eqDoWhile: Eq[DoWhile] = semiauto.eq

  case class For(foreign: tinyc.ASTFor) extends AST
  implicit val eqForeignASTFor: Eq[tinyc.ASTFor] = Eq.fromUniversalEquals
  implicit val eqFor: Eq[For] = semiauto.eq

  case class Break(foreign: tinyc.ASTBreak) extends AST
  implicit val eqForeignASTBreak: Eq[tinyc.ASTBreak] = Eq.fromUniversalEquals
  implicit val eqBreak: Eq[Break] = semiauto.eq

  case class Continue(foreign: tinyc.ASTContinue) extends AST
  implicit val eqForeignASTContinue: Eq[tinyc.ASTContinue] = Eq.fromUniversalEquals
  implicit val eqContinue: Eq[Continue] = semiauto.eq

  case class Return(foreign: tinyc.ASTReturn) extends AST {
    lazy val value: AST = Option(foreign.getValue).get.wrapped
  }
  implicit val eqForeignASTReturn: Eq[tinyc.ASTReturn] = Eq.fromUniversalEquals
  implicit val eqReturn: Eq[Return] = semiauto.eq

  case class BinaryOp(foreign: tinyc.ASTBinaryOp) extends AST {
    lazy val left: AST = Option(foreign.getLeft).get.wrapped
    lazy val op: tinyc.Symbol = Option(foreign.getOp).get
    lazy val right: AST = Option(foreign.getRight).get.wrapped
  }
  implicit val eqForeignASTBinaryOp: Eq[tinyc.ASTBinaryOp] = Eq.fromUniversalEquals
  implicit val eqBinaryOp: Eq[BinaryOp] = semiauto.eq

  case class Assignment(foreign: tinyc.ASTAssignment) extends AST {
    lazy val lvalue: AST = foreign.getLvalue.wrapped
    lazy val rvalue: AST = foreign.getValue.wrapped
  }
  implicit val eqForeignASTAssignment: Eq[tinyc.ASTAssignment] = Eq.fromUniversalEquals
  implicit val eqAssignment: Eq[Assignment] = semiauto.eq

  case class UnaryOp(foreign: tinyc.ASTUnaryOp) extends AST
  implicit val eqForeignASTUnaryOp: Eq[tinyc.ASTUnaryOp] = Eq.fromUniversalEquals
  implicit val eqUnaryOp: Eq[UnaryOp] = semiauto.eq

  case class UnaryPostOp(foreign: tinyc.ASTUnaryPostOp) extends AST
  implicit val eqForeignASTUnaryPostOp: Eq[tinyc.ASTUnaryPostOp] = Eq.fromUniversalEquals
  implicit val eqUnaryPostOp: Eq[UnaryPostOp] = semiauto.eq

  case class Address(foreign: tinyc.ASTAddress) extends AST
  implicit val eqForeignASTAddress: Eq[tinyc.ASTAddress] = Eq.fromUniversalEquals
  implicit val eqAddress: Eq[Address] = semiauto.eq

  case class Deref(foreign: tinyc.ASTDeref) extends AST
  implicit val eqForeignASTDeref: Eq[tinyc.ASTDeref] = Eq.fromUniversalEquals
  implicit val eqDeref: Eq[Deref] = semiauto.eq

  case class Index(foreign: tinyc.ASTIndex) extends AST
  implicit val eqForeignASTIndex: Eq[tinyc.ASTIndex] = Eq.fromUniversalEquals
  implicit val eqIndex: Eq[Index] = semiauto.eq

  case class Member(foreign: tinyc.ASTMember) extends AST
  implicit val eqForeignASTMember: Eq[tinyc.ASTMember] = Eq.fromUniversalEquals
  implicit val eqMember: Eq[Member] = semiauto.eq

  case class MemberPtr(foreign: tinyc.ASTMemberPtr) extends AST
  implicit val eqForeignASTMemberPtr: Eq[tinyc.ASTMemberPtr] = Eq.fromUniversalEquals
  implicit val eqMemberPtr: Eq[MemberPtr] = semiauto.eq

  case class Call(foreign: tinyc.ASTCall) extends AST
  implicit val eqForeignASTCall: Eq[tinyc.ASTCall] = Eq.fromUniversalEquals
  implicit val eqCall: Eq[Call] = semiauto.eq

  case class Cast(foreign: tinyc.ASTCast) extends AST
  implicit val eqForeignASTCast: Eq[tinyc.ASTCast] = Eq.fromUniversalEquals
  implicit val eqCast: Eq[Cast] = semiauto.eq

  case class Write(foreign: tinyc.ASTWrite) extends AST
  implicit val eqForeignASTWrite: Eq[tinyc.ASTWrite] = Eq.fromUniversalEquals
  implicit val eqWrite: Eq[Write] = semiauto.eq

  case class Read(foreign: tinyc.ASTRead) extends AST
  implicit val eqForeignASTRead: Eq[tinyc.ASTRead] = Eq.fromUniversalEquals
  implicit val eqRead: Eq[Read] = semiauto.eq
}
