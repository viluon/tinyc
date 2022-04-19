package me.viluon.tinyc
package bindings

import cats.syntax.all._

// Courtesy of @MrVoltz#5724
object ASTBridge {

  private class Impl_ extends tinyc.ASTVisitor {
    var wrapped: Option[Either[Type, AST]] = None

    override def visit(ast: tinyc.AST): Unit = {
      println("generic")
      wrapped = None
    }

    override def visit(astNamedType: tinyc.ASTNamedType): Unit = wrapped = (astNamedType.getName.name() match {
      case "int" => Type.Int()
      case "void" => Type.Unit()
      case "char" => Type.Char()
      case "double" => Type.Double()
      case t => throw new IllegalStateException(s"unknown type $t")
    }).asLeft.some
    override def visit(astType: tinyc.ASTType): Unit = {
      println("Type")
      ???
    }
    override def visit(astPointerType: tinyc.ASTPointerType): Unit = {
      println("PointerType")
      ???
    }
    override def visit(astArrayType: tinyc.ASTArrayType): Unit = {
      println("ArrayType")
      ???
    }

    override def visit(astInteger: tinyc.ASTInteger): Unit = {
      println("Integer")
      wrapped = AST.Integer(astInteger).asRight.some
    }
    override def visit(astDouble: tinyc.ASTDouble): Unit = {
      println("Double")
      wrapped = AST.Double(astDouble).asRight.some
    }
    override def visit(astChar: tinyc.ASTChar): Unit = {
      println("Char")
      wrapped = AST.Char(astChar).asRight.some
    }
    override def visit(astString: tinyc.ASTString): Unit = {
      println("String")
      wrapped = AST.String(astString).asRight.some
    }
    override def visit(astIdentifier: tinyc.ASTIdentifier): Unit = {
      println("Identifier")
      wrapped = AST.Identifier(astIdentifier).asRight.some
    }
    override def visit(astSequence: tinyc.ASTSequence): Unit = {
      println("Sequence")
      wrapped = AST.Sequence(astSequence).asRight.some
    }
    override def visit(astBlock: tinyc.ASTBlock): Unit = {
      println("here")
      if (astBlock == null) throw new NullPointerException()
//      val field = astBlock.getClass.getField("swigCPtr")
//      println("got the field")
//      field.setAccessible(true)
//      println(field.get(astBlock))
      println(s"look: $astBlock")
      wrapped = AST.Block(astBlock).asRight.some
    }

    override def visit(astVarDecl: tinyc.ASTVarDecl): Unit = {
      println("VarDecl")
      wrapped = AST.VarDecl(astVarDecl).asRight.some
    }
    override def visit(astFunDecl: tinyc.ASTFunDecl): Unit = {
      println("FunDecl")
      wrapped = AST.FunDecl(astFunDecl).asRight.some
    }
    override def visit(astStructDecl: tinyc.ASTStructDecl): Unit = {
      println("StructDecl")
      wrapped = AST.StructDecl(astStructDecl).asRight.some
    }
    override def visit(astFunPtrDecl: tinyc.ASTFunPtrDecl): Unit = {
      println("FunPtrDecl")
      wrapped = AST.FunPtrDecl(astFunPtrDecl).asRight.some
    }
    override def visit(astIf: tinyc.ASTIf): Unit = {
      println("If")
      wrapped = AST.If(astIf).asRight.some
    }
    override def visit(astSwitch: tinyc.ASTSwitch): Unit = {
      println("Switch")
      wrapped = AST.Switch(astSwitch).asRight.some
    }
    override def visit(astWhile: tinyc.ASTWhile): Unit = {
      println("While")
      wrapped = AST.While(astWhile).asRight.some
    }
    override def visit(astDoWhile: tinyc.ASTDoWhile): Unit = {
      println("DoWhile")
      wrapped = AST.DoWhile(astDoWhile).asRight.some
    }
    override def visit(astFor: tinyc.ASTFor): Unit = {
      println("For")
      wrapped = AST.For(astFor).asRight.some
    }
    override def visit(astBreak: tinyc.ASTBreak): Unit = {
      println("Break")
      wrapped = AST.Break(astBreak).asRight.some
    }
    override def visit(astContinue: tinyc.ASTContinue): Unit = {
      println("Continue")
      wrapped = AST.Continue(astContinue).asRight.some
    }
    override def visit(astReturn: tinyc.ASTReturn): Unit = {
      println("Return")
      wrapped = AST.Return(astReturn).asRight.some
    }
    override def visit(astBinaryOp: tinyc.ASTBinaryOp): Unit = {
      println("BinaryOp")
      wrapped = AST.BinaryOp(astBinaryOp).asRight.some
    }
    override def visit(astAssignment: tinyc.ASTAssignment): Unit = {
      println("Assignment")
      wrapped = AST.Assignment(astAssignment).asRight.some
    }
    override def visit(astUnaryOp: tinyc.ASTUnaryOp): Unit = {
      println("UnaryOp")
      wrapped = AST.UnaryOp(astUnaryOp).asRight.some
    }
    override def visit(astUnaryPostOp: tinyc.ASTUnaryPostOp): Unit = {
      println("UnaryPostOp")
      wrapped = AST.UnaryPostOp(astUnaryPostOp).asRight.some
    }
    override def visit(astAddress: tinyc.ASTAddress): Unit = {
      println("Address")
      wrapped = AST.Address(astAddress).asRight.some
    }
    override def visit(astDeref: tinyc.ASTDeref): Unit = {
      println("Deref")
      wrapped = AST.Deref(astDeref).asRight.some
    }
    override def visit(astIndex: tinyc.ASTIndex): Unit = {
      println("Index")
      wrapped = AST.Index(astIndex).asRight.some
    }
    override def visit(astMember: tinyc.ASTMember): Unit = {
      println("Member")
      wrapped = AST.Member(astMember).asRight.some
    }
    override def visit(astMemberPtr: tinyc.ASTMemberPtr): Unit = {
      println("MemberPtr")
      wrapped = AST.MemberPtr(astMemberPtr).asRight.some
    }
    override def visit(astCall: tinyc.ASTCall): Unit = {
      println("Call")
      wrapped = AST.Call(astCall).asRight.some
    }
    override def visit(astCast: tinyc.ASTCast): Unit = {
      println("Cast")
      wrapped = AST.Cast(astCast).asRight.some
    }
    override def visit(astWrite: tinyc.ASTWrite): Unit = {
      println("Write")
      wrapped = AST.Write(astWrite).asRight.some
    }
    override def visit(astRead: tinyc.ASTRead): Unit = {
      println("Read")
      wrapped = AST.Read(astRead).asRight.some
    }

    override def visitChild(ast: tinyc.AST): Unit = {
      if (ast == null) throw new NullPointerException()
      super.visitChild(ast)
    }

    def wrap(node: tinyc.AST): Either[Type, AST] = {
      if (node == null) throw new NullPointerException()
      visitChild(node)
      wrapped.get
    }
  }

  def wrap(node: tinyc.AST): Either[Type, AST] = (new Impl_).wrap(node)

  implicit class ForeignASTBridgeOps(node: tinyc.AST) {
    def wrapped: AST = ASTBridge.wrap(node).toOption.get
  }

  implicit class ForeignTypeBridgeOps(t: tinyc.ASTType) {
    def nativeType: Type = ASTBridge.wrap(t).left.toOption.get
  }
}
