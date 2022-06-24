package me.viluon.tinyc

import bindings.ASTBridge.ForeignASTBridgeOps

import cats.data.{EitherT, State}
import bindings.AST

import cats.syntax.eq._
import cats.syntax.show._
import Type.eqType

import cats.implicits.toTraverseOps
import cats.Show
import me.viluon.tinyc.{Type => NativeType}

// I use "native" to mean "native to this codebase." Things that come from C++
// (via JNI) are typically named "foreign." I believe "native methods" are a misnomer.
// (Otherwise, how is one to call non-native methods? Certainly not "foreign," right?)

object TypeAnalysis {
  case class TypeAnalysisState(declarations: Map[tinyc.Symbol, NativeType])
  case class TypeError(msg: String, location: tinyc.SourceLocation)
  implicit object TypeErrorShow extends Show[TypeError] {
    override def show(t: TypeError): String = {
      val (file, line, col) = (t.location.file(), t.location.line(), t.location.col())
      s"$file:$line:$col: ${t.msg}"
    }
  }

  type Analysis[A] = EitherT[State[TypeAnalysisState, *], List[TypeError], A]

  private def assign(sym: tinyc.Symbol, t: NativeType): Analysis[Unit] = EitherT.liftF(State.modify(
    s => s.copy(declarations = s.declarations + (sym -> t))
  ))

  private def query(sym: tinyc.Symbol): Analysis[Option[NativeType]] = EitherT.liftF(State.inspect(
    s => s.declarations.get(sym)
  ))

  private def get: Analysis[TypeAnalysisState] = EitherT.liftF(State.get)

  @inline private def pure[A](a: A): Analysis[A] = EitherT.liftF(State.pure(a))
  @inline private def crash[A](msg: String, loc: tinyc.SourceLocation): Analysis[A] =
    EitherT.leftT(List(TypeError(msg, loc)))

  @inline private def expect(b: Boolean, msg: => String, loc: => tinyc.SourceLocation): Analysis[Unit] =
    if (b) pure(()) else crash(msg, loc)

  def analyse(ast: tinyc.AST): Either[List[TypeError], Map[tinyc.Symbol, Type]] =
    analyse(ast.wrapped).flatMap(_ => get).value.run(TypeAnalysisState(Map())).value._2.map(_.declarations)

  def analyse(ast: AST): Analysis[NativeType] = ast match {
    case _: AST.Integer => pure(NativeType.Int())
    case _: AST.Double => pure(NativeType.Double())
    case _: AST.Char => pure(NativeType.Char())
    case _: AST.String => pure(NativeType.String())
    case id: AST.Identifier => for {
      mt <- query(id.symbol)
      t <- mt match {
        case Some(t) => pure(t)
        case None => crash(s"undefined $id", id.loc)
      }
    } yield t
    case _: AST.Type => ???
    case _: AST.PointerType => ???
    case _: AST.ArrayType => ???
    case _: AST.NamedType => ???
    case seq: AST.Sequence => seq.body.traverse(analyse).map(_.lastOption.getOrElse(Type.Unit()))
    case block: AST.Block => block.stmts.traverse(analyse).map(_.lastOption.getOrElse(Type.Unit()))
    case decl: AST.VarDecl => for {
      _ <- assign(decl.ident.symbol, decl.typ)
      t <- analyse(decl.value)
      _ <- expect(
        decl.typ === t,
        s"Expected ${decl.typ.show}, got ${t.show}",
        decl.loc
      )
    } yield NativeType.Unit()
    case fn: AST.FunDecl => for {
      // TODO: add params to the scope
      t <- analyse(fn.body)
      _ <- expect(
        fn.returnType === t,
        s"Expected ${fn.returnType.show}, got ${t.show}",
        // FIXME: return could appear elsewhere
        fn.body.stmts.last.loc
      )
    } yield NativeType.Unit()
    case _: AST.StructDecl => ???
    case _: AST.FunPtrDecl => ???
    case br: AST.If => for {
      t <- analyse(br.condition)
      _ <- expect(
        t === NativeType.Int(),
        s"If statements can only branch on integers, not on $t",
        br.condition.loc
      )
      // TODO: scoping?
      _ <- analyse(br.consequent)
      _ <- br.alternative.traverse(analyse)
    } yield NativeType.Unit()
    case _: AST.Switch => ???
    case loop: AST.While => for {
      t <- analyse(loop.condition)
      _ <- expect(
        t === NativeType.Int(),
        s"While statements can only branch on integers, not on $t",
        loop.condition.loc
      )
      _ <- analyse(loop.body)
    } yield NativeType.Unit()
    case _: AST.DoWhile => ???
    case _: AST.For => ???
    case _: AST.Break => ???
    case _: AST.Continue => ???
    case ret: AST.Return => analyse(ret.value)
    case bin: AST.BinaryOp => for {
      tl <- analyse(bin.left)
      tr <- analyse(bin.right)
      _ <- expect(
        tl === tr,
        s"Arguments to '${bin.op}' must have identical types, $tl and $tr are not identical.",
        bin.loc
      )
    } yield tl
    case a: AST.Assignment => for {
      tl <- analyse(a.lvalue)
      tr <- analyse(a.rvalue)
      _ <- expect(
        tl === tr,
        s"Cannot assign $tr to an lvalue of type $tl",
        a.loc
      )
    } yield tr
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
