package me.viluon.tinyc
package bindings

import cats.syntax.all._

// Courtesy of @MrVoltz#5724
object ASTTypeResolver {

  private class Impl_ extends tinyc.ASTVisitor {
    var wrapped: Option[AST] = None

    override def visit(ast: tinyc.AST): Unit = wrapped = None

    override def visit(astInteger: tinyc.ASTInteger): Unit = wrapped = AST.Integer(astInteger).some
    override def visit(astDouble: tinyc.ASTDouble): Unit = wrapped = AST.Double(astDouble).some
    override def visit(astChar: tinyc.ASTChar): Unit = wrapped = AST.Char(astChar).some
    override def visit(astString: tinyc.ASTString): Unit = wrapped = AST.String(astString).some
    override def visit(astIdentifier: tinyc.ASTIdentifier): Unit = wrapped = AST.Identifier(astIdentifier).some
    override def visit(astType: tinyc.ASTType): Unit = wrapped = AST.Type(astType).some
    override def visit(astPointerType: tinyc.ASTPointerType): Unit = wrapped = AST.PointerType(astPointerType).some
    override def visit(astArrayType: tinyc.ASTArrayType): Unit = wrapped = AST.ArrayType(astArrayType).some
    override def visit(astNamedType: tinyc.ASTNamedType): Unit = wrapped = AST.NamedType(astNamedType).some
    override def visit(astSequence: tinyc.ASTSequence): Unit = wrapped = AST.Sequence(astSequence).some
    override def visit(astBlock: tinyc.ASTBlock): Unit = wrapped = AST.Block(astBlock).some
    override def visit(astVarDecl: tinyc.ASTVarDecl): Unit = wrapped = AST.VarDecl(astVarDecl).some
    override def visit(astFunDecl: tinyc.ASTFunDecl): Unit = wrapped = AST.FunDecl(astFunDecl).some
    override def visit(astStructDecl: tinyc.ASTStructDecl): Unit = wrapped = AST.StructDecl(astStructDecl).some
    override def visit(astFunPtrDecl: tinyc.ASTFunPtrDecl): Unit = wrapped = AST.FunPtrDecl(astFunPtrDecl).some
    override def visit(astIf: tinyc.ASTIf): Unit = wrapped = AST.If(astIf).some
    override def visit(astSwitch: tinyc.ASTSwitch): Unit = wrapped = AST.Switch(astSwitch).some
    override def visit(astWhile: tinyc.ASTWhile): Unit = wrapped = AST.While(astWhile).some
    override def visit(astDoWhile: tinyc.ASTDoWhile): Unit = wrapped = AST.DoWhile(astDoWhile).some
    override def visit(astFor: tinyc.ASTFor): Unit = wrapped = AST.For(astFor).some
    override def visit(astBreak: tinyc.ASTBreak): Unit = wrapped = AST.Break(astBreak).some
    override def visit(astContinue: tinyc.ASTContinue): Unit = wrapped = AST.Continue(astContinue).some
    override def visit(astReturn: tinyc.ASTReturn): Unit = wrapped = AST.Return(astReturn).some
    override def visit(astBinaryOp: tinyc.ASTBinaryOp): Unit = wrapped = AST.BinaryOp(astBinaryOp).some
    override def visit(astAssignment: tinyc.ASTAssignment): Unit = wrapped = AST.Assignment(astAssignment).some
    override def visit(astUnaryOp: tinyc.ASTUnaryOp): Unit = wrapped = AST.UnaryOp(astUnaryOp).some
    override def visit(astUnaryPostOp: tinyc.ASTUnaryPostOp): Unit = wrapped = AST.UnaryPostOp(astUnaryPostOp).some
    override def visit(astAddress: tinyc.ASTAddress): Unit = wrapped = AST.Address(astAddress).some
    override def visit(astDeref: tinyc.ASTDeref): Unit = wrapped = AST.Deref(astDeref).some
    override def visit(astIndex: tinyc.ASTIndex): Unit = wrapped = AST.Index(astIndex).some
    override def visit(astMember: tinyc.ASTMember): Unit = wrapped = AST.Member(astMember).some
    override def visit(astMemberPtr: tinyc.ASTMemberPtr): Unit = wrapped = AST.MemberPtr(astMemberPtr).some
    override def visit(astCall: tinyc.ASTCall): Unit = wrapped = AST.Call(astCall).some
    override def visit(astCast: tinyc.ASTCast): Unit = wrapped = AST.Cast(astCast).some
    override def visit(astWrite: tinyc.ASTWrite): Unit = wrapped = AST.Write(astWrite).some
    override def visit(astRead: tinyc.ASTRead): Unit = wrapped = AST.Read(astRead).some

    def wrap(node: tinyc.AST): AST = {
      visitChild(node)
      wrapped.get
    }
  }

  def wrap(node: tinyc.AST): AST = (new Impl_).wrap(node)

  implicit class ASTTypeResolverExtensions(node: tinyc.AST) {
    def wrapped: AST = ASTTypeResolver.wrap(node)
  }

}
